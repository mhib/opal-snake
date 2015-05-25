(function(undefined) {
  if (typeof(this.Opal) !== 'undefined') {
    console.warn('Opal already loaded. Loading twice can cause troubles, please fix your setup.');
    return this.Opal;
  }

  // The Opal object that is exposed globally
  var Opal = this.Opal = {};

  // All bridged classes - keep track to donate methods from Object
  var bridged_classes = Opal.bridged_classes = [];

  // TopScope is used for inheriting constants from the top scope
  var TopScope = function(){};

  // Opal just acts as the top scope
  TopScope.prototype = Opal;

  // To inherit scopes
  Opal.constructor = TopScope;

  // List top scope constants
  Opal.constants = [];

  // This is a useful reference to global object inside ruby files
  Opal.global = this;

  // Minify common function calls
  var $hasOwn = Opal.hasOwnProperty;
  var $slice  = Opal.slice = Array.prototype.slice;

  // Generates unique id for every ruby object
  var unique_id = 0;

  // Return next unique id
  Opal.uid = function() {
    return unique_id++;
  };

  // Table holds all class variables
  Opal.cvars = {};

  // Globals table
  Opal.gvars = {};

  // Exit function, this should be replaced by platform specific implementation
  // (See nodejs and phantom for examples)
  Opal.exit = function(status) { if (Opal.gvars.DEBUG) console.log('Exited with status '+status); };

  /**
    Get a constant on the given scope. Every class and module in Opal has a
    scope used to store, and inherit, constants. For example, the top level
    `Object` in ruby has a scope accessible as `Opal.Object.$$scope`.

    To get the `Array` class using this scope, you could use:

        Opal.Object.$$scope.get("Array")

    If a constant with the given name cannot be found, then a dispatch to the
    class/module's `#const_method` is called, which by default will raise an
    error.

    @param [String] name the name of the constant to lookup
    @returns [RubyObject]
  */
  Opal.get = function(name) {
    var constant = this[name];

    if (constant == null) {
      return this.base.$const_missing(name);
    }

    return constant;
  };

  /*
   * Create a new constants scope for the given class with the given
   * base. Constants are looked up through their parents, so the base
   * scope will be the outer scope of the new klass.
   */
  function create_scope(base, klass, id) {
    var const_alloc = function() {};
    var const_scope = const_alloc.prototype = new base.constructor();

    klass.$$scope       = const_scope;
    klass.$$base_module = base.base;

    const_scope.base        = klass;
    const_scope.constructor = const_alloc;
    const_scope.constants   = [];

    if (id) {
      klass.$$orig_scope = base;
      base[id] = base.constructor[id] = klass;
      base.constants.push(id);
    }
  }

  Opal.create_scope = create_scope;

  /*
   * A `class Foo; end` expression in ruby is compiled to call this runtime
   * method which either returns an existing class of the given name, or creates
   * a new class in the given `base` scope.
   *
   * If a constant with the given name exists, then we check to make sure that
   * it is a class and also that the superclasses match. If either of these
   * fail, then we raise a `TypeError`. Note, superklass may be null if one was
   * not specified in the ruby code.
   *
   * We pass a constructor to this method of the form `function ClassName() {}`
   * simply so that classes show up with nicely formatted names inside debuggers
   * in the web browser (or node/sprockets).
   *
   * The `base` is the current `self` value where the class is being created
   * from. We use this to get the scope for where the class should be created.
   * If `base` is an object (not a class/module), we simple get its class and
   * use that as the base instead.
   *
   * @param [Object] base where the class is being created
   * @param [Class] superklass superclass of the new class (may be null)
   * @param [String] id the name of the class to be created
   * @param [Function] constructor function to use as constructor
   * @return [Class] new or existing ruby class
   */
  Opal.klass = function(base, superklass, id, constructor) {
    // If base is an object, use its class
    if (!base.$$is_class) {
      base = base.$$class;
    }

    // Not specifying a superclass means we can assume it to be Object
    if (superklass === null) {
      superklass = ObjectClass;
    }

    var klass = base.$$scope[id];

    // If a constant exists in the scope, then we must use that
    if ($hasOwn.call(base.$$scope, id) && klass.$$orig_scope === base.$$scope) {
      // Make sure the existing constant is a class, or raise error
      if (!klass.$$is_class) {
        throw Opal.TypeError.$new(id + " is not a class");
      }

      // Make sure existing class has same superclass
      if (superklass !== klass.$$super && superklass !== ObjectClass) {
        throw Opal.TypeError.$new("superclass mismatch for class " + id);
      }
    }
    else if (typeof(superklass) === 'function') {
      // passed native constructor as superklass, so bridge it as ruby class
      return bridge_class(id, superklass);
    }
    else {
      // if class doesnt exist, create a new one with given superclass
      klass = boot_class(superklass, constructor);

      // name class using base (e.g. Foo or Foo::Baz)
      klass.$$name = id;

      // every class gets its own constant scope, inherited from current scope
      create_scope(base.$$scope, klass, id);

      // Name new class directly onto current scope (Opal.Foo.Baz = klass)
      base[id] = base.$$scope[id] = klass;

      // Copy all parent constants to child, unless parent is Object
      if (superklass !== ObjectClass && superklass !== BasicObjectClass) {
        donate_constants(superklass, klass);
      }

      // call .inherited() hook with new class on the superclass
      if (superklass.$inherited) {
        superklass.$inherited(klass);
      }
    }

    return klass;
  };

  // Create generic class with given superclass.
  function boot_class(superklass, constructor) {
    var alloc = boot_class_alloc(null, constructor, superklass)

    return boot_class_object(superklass, alloc);
  }

  // Make `boot_class` available to the JS-API
  Opal.boot = boot_class;

  /*
   * The class object itself (as in `Class.new`)
   *
   * @param [(Opal) Class] superklass Another class object (as in `Class.new`)
   * @param [constructor]  alloc      The constructor that holds the prototype
   *                                  that will be used for instances of the
   *                                  newly constructed class.
   */
  function boot_class_object(superklass, alloc) {
    var singleton_class = function() {};
    singleton_class.prototype = superklass.constructor.prototype;

    function OpalClass() {}
    OpalClass.prototype = new singleton_class();

    var klass = new OpalClass();

    setup_module_or_class_object(klass, OpalClass, superklass, alloc.prototype);

    // @property $$alloc This is the constructor of instances of the current
    //                   class. Its prototype will be used for method lookup
    klass.$$alloc = alloc;

    // @property $$proto.$$class Make available to instances a reference to the
    //                           class they belong to.
    klass.$$proto.$$class = klass;

    return klass;
  }

  /*
   * Adds common/required properties to a module or class object
   * (as in `Module.new` / `Class.new`)
   *
   * @param module      The module or class that needs to be prepared
   *
   * @param constructor The constructor of the module or class itself,
   *                    usually it's already assigned by using `new`. Some
   *                    ipothesis on why it's needed can be found below.
   *
   * @param superklass  The superclass of the class/module object, for modules
   *                    is `Module` (of `ModuleClass` in JS context)
   *
   * @param prototype   The prototype on which the class/module methods will
   *                    be stored.
   */
  function setup_module_or_class_object(module, constructor, superklass, prototype) {
    // @property $$id Each class is assigned a unique `id` that helps
    //                comparation and implementation of `#object_id`
    module.$$id = unique_id++;

    // @property $$proto This is the prototype on which methods will be defined
    module.$$proto = prototype;

    // @property constructor keeps a ref to the constructor, but apparently the
    //                       constructor is already set on:
    //
    //                          `var module = new constructor` is called.
    //
    //                       Maybe there are some browsers not abiding (IE6?)
    module.constructor = constructor;

    // @property $$is_class Clearly mark this as a class-like
    module.$$is_class = true;

    // @property $$super the superclass, doesn't get changed by module inclusions
    module.$$super = superklass;

    // @property $$parent direct parent class or module
    //                    starts with the superclass, after module inclusion is
    //                    the last included module
    module.$$parent = superklass;

    // @property $$methods keeps track of methods defined on the class
    //                     but seems to be used just by `define_basic_object_method`
    //                     and for donating (Ruby) Object methods to bridged classes
    //                     TODO: check if it can be removed
    module.$$methods = [];

    // @property $$inc included modules
    module.$$inc = [];
  }

  /**
    Define new module (or return existing module). The given `base` is basically
    the current `self` value the `module` statement was defined in. If this is
    a ruby module or class, then it is used, otherwise if the base is a ruby
    object then that objects real ruby class is used (e.g. if the base is the
    main object, then the top level `Object` class is used as the base).

    If a module of the given name is already defined in the base, then that
    instance is just returned.

    If there is a class of the given name in the base, then an error is
    generated instead (cannot have a class and module of same name in same base).

    Otherwise, a new module is created in the base with the given name, and that
    new instance is returned back (to be referenced at runtime).

    @param [RubyModule or Class] base class or module this definition is inside
    @param [String] id the name of the new (or existing) module
    @returns [RubyModule]
  */
  Opal.module = function(base, id) {
    var module;

    if (!base.$$is_class) {
      base = base.$$class;
    }

    if ($hasOwn.call(base.$$scope, id)) {
      module = base.$$scope[id];

      if (!module.$$is_mod && module !== ObjectClass) {
        throw Opal.TypeError.$new(id + " is not a module");
      }
    }
    else {
      module = boot_module_object();
      module.$$name = id;

      create_scope(base.$$scope, module, id);

      // Name new module directly onto current scope (Opal.Foo.Baz = module)
      base[id] = base.$$scope[id] = module;
    }

    return module;
  };

  /*
   * Internal function to create a new module instance. This simply sets up
   * the prototype hierarchy and method tables.
   */
  function boot_module_object() {
    var mtor = function() {};
    mtor.prototype = ModuleClass.constructor.prototype;

    function module_constructor() {}
    module_constructor.prototype = new mtor();

    var module = new module_constructor();
    var module_prototype = {};

    setup_module_or_class_object(module, module_constructor, ModuleClass, module_prototype);

    module.$$is_mod = true;
    module.$$dep    = [];

    return module;
  }

  /**
    Return the singleton class for the passed object.

    If the given object alredy has a singleton class, then it will be stored on
    the object as the `$$meta` property. If this exists, then it is simply
    returned back.

    Otherwise, a new singleton object for the class or object is created, set on
    the object at `$$meta` for future use, and then returned.

    @param [RubyObject] object the ruby object
    @returns [RubyClass] the singleton class for object
  */
  Opal.get_singleton_class = function(object) {
    if (object.$$meta) {
      return object.$$meta;
    }

    if (object.$$is_class) {
      return build_class_singleton_class(object);
    }

    return build_object_singleton_class(object);
  };

  /**
    Build the singleton class for an existing class.

    NOTE: Actually in MRI a class' singleton class inherits from its
    superclass' singleton class which in turn inherits from Class.

    @param [RubyClass] klass
    @returns [RubyClass]
   */
  function build_class_singleton_class(klass) {
    var meta = new Opal.Class.$$alloc;

    meta.$$class = Opal.Class;
    meta.$$proto = klass.constructor.prototype;

    meta.$$is_singleton = true;
    meta.$$inc          = [];
    meta.$$methods      = [];
    meta.$$scope        = klass.$$scope;

    return klass.$$meta = meta;
  }

  /**
    Build the singleton class for a Ruby (non class) Object.

    @param [RubyObject] object
    @returns [RubyClass]
   */
  function build_object_singleton_class(object) {
    var orig_class = object.$$class,
        class_id   = "#<Class:#<" + orig_class.$$name + ":" + orig_class.$$id + ">>";

    var Singleton = function () {};
    var meta = Opal.boot(orig_class, Singleton);
    meta.$$name   = class_id;

    meta.$$proto  = object;
    meta.$$class  = orig_class.$$class;
    meta.$$scope  = orig_class.$$scope;
    meta.$$parent = orig_class;
    return object.$$meta = meta;
  }

  /**
    The actual inclusion of a module into a class.

    ## Class `$$parent` and `iclass`

    To handle `super` calls, every class has a `$$parent`. This parent is
    used to resolve the next class for a super call. A normal class would
    have this point to its superclass. However, if a class includes a module
    then this would need to take into account the module. The module would
    also have to then point its `$$parent` to the actual superclass. We
    cannot modify modules like this, because it might be included in more
    then one class. To fix this, we actually insert an `iclass` as the class'
    `$$parent` which can then point to the superclass. The `iclass` acts as
    a proxy to the actual module, so the `super` chain can then search it for
    the required method.

    @param [RubyModule] module the module to include
    @param [RubyClass] klass the target class to include module into
    @returns [null]
  */
  Opal.append_features = function(module, klass) {
    var included = klass.$$inc;

    // check if this module is already included in the klass
    for (var j = 0, jj = included.length; j < jj; j++) {
      if (included[j] === module) {
        return;
      }
    }

    included.push(module);
    module.$$dep.push(klass);

    // iclass
    var iclass = {
      $$name:   module.$$name,
      $$proto:  module.$$proto,
      $$parent: klass.$$parent,
      $$module: module,
      $$iclass: true
    };

    klass.$$parent = iclass;

    var donator   = module.$$proto,
        prototype = klass.$$proto,
        methods   = module.$$methods;

    for (var i = 0, length = methods.length; i < length; i++) {
      var method = methods[i], current;


      if ( prototype.hasOwnProperty(method) &&
          !(current = prototype[method]).$$donated && !current.$$stub ) {
        // if the target class already has a method of the same name defined
        // and that method was NOT donated, then it must be a method defined
        // by the class so we do not want to override it
      }
      else {
        prototype[method] = donator[method];
        prototype[method].$$donated = true;
      }
    }

    if (klass.$$dep) {
      donate_methods(klass, methods.slice(), true);
    }

    donate_constants(module, klass);
  };

  // Boot a base class (makes instances).
  function boot_class_alloc(id, constructor, superklass) {
    if (superklass) {
      var ctor = function() {};
          ctor.prototype   = superklass.$$proto || superklass.prototype;

      if (id) {
        ctor.displayName = id;
      }

      constructor.prototype = new ctor();
    }

    constructor.prototype.constructor = constructor;

    return constructor;
  }

  /*
   * Builds the class object for core classes:
   * - make the class object have a singleton class
   * - make the singleton class inherit from its parent singleton class
   *
   * @param id         [String]      the name of the class
   * @param alloc      [Function]    the constructor for the core class instances
   * @param superclass [Class alloc] the constructor of the superclass
   */
  function boot_core_class_object(id, alloc, superclass) {
    var superclass_constructor = function() {};
        superclass_constructor.prototype = superclass.prototype;

    var singleton_class = function() {};
        singleton_class.prototype = new superclass_constructor();

    singleton_class.displayName = "#<Class:"+id+">";

    // the singleton_class acts as the class object constructor
    var klass = new singleton_class();

    setup_module_or_class_object(klass, singleton_class, superclass, alloc.prototype);

    klass.$$alloc = alloc;
    klass.$$name  = id;

    // Give all instances a ref to their class
    alloc.prototype.$$class = klass;

    Opal[id] = klass;
    Opal.constants.push(id);

    return klass;
  }

  /*
   * For performance, some core ruby classes are toll-free bridged to their
   * native javascript counterparts (e.g. a ruby Array is a javascript Array).
   *
   * This method is used to setup a native constructor (e.g. Array), to have
   * its prototype act like a normal ruby class. Firstly, a new ruby class is
   * created using the native constructor so that its prototype is set as the
   * target for th new class. Note: all bridged classes are set to inherit
   * from Object.
   *
   * Bridged classes are tracked in `bridged_classes` array so that methods
   * defined on Object can be "donated" to all bridged classes. This allows
   * us to fake the inheritance of a native prototype from our Object
   * prototype.
   *
   * Example:
   *
   *    bridge_class("Proc", Function);
   *
   * @param [String] name the name of the ruby class to create
   * @param [Function] constructor native javascript constructor to use
   * @return [Class] returns new ruby class
   */
  function bridge_class(name, constructor) {
    var klass = boot_class_object(ObjectClass, constructor);

    klass.$$name = name;

    create_scope(Opal, klass, name);
    bridged_classes.push(klass);

    var object_methods = BasicObjectClass.$$methods.concat(ObjectClass.$$methods);

    for (var i = 0, len = object_methods.length; i < len; i++) {
      var meth = object_methods[i];
      constructor.prototype[meth] = ObjectClass.$$proto[meth];
    }

    add_stubs_subscriber(constructor.prototype);

    return klass;
  }

  /*
   * constant assign
   */
  Opal.casgn = function(base_module, name, value) {
    var scope = base_module.$$scope;

    if (value.$$is_class && value.$$name === nil) {
      value.$$name = name;
    }

    if (value.$$is_class) {
      value.$$base_module = base_module;
    }

    scope.constants.push(name);
    return scope[name] = value;
  };

  /*
   * constant decl
   */
  Opal.cdecl = function(base_scope, name, value) {
    base_scope.constants.push(name);
    return base_scope[name] = value;
  };

  /*
   * When a source module is included into the target module, we must also copy
   * its constants to the target.
   */
  function donate_constants(source_mod, target_mod) {
    var source_constants = source_mod.$$scope.constants,
        target_scope     = target_mod.$$scope,
        target_constants = target_scope.constants;

    for (var i = 0, length = source_constants.length; i < length; i++) {
      target_constants.push(source_constants[i]);
      target_scope[source_constants[i]] = source_mod.$$scope[source_constants[i]];
    }
  };

  /*
   * Methods stubs are used to facilitate method_missing in opal. A stub is a
   * placeholder function which just calls `method_missing` on the receiver.
   * If no method with the given name is actually defined on an object, then it
   * is obvious to say that the stub will be called instead, and then in turn
   * method_missing will be called.
   *
   * When a file in ruby gets compiled to javascript, it includes a call to
   * this function which adds stubs for every method name in the compiled file.
   * It should then be safe to assume that method_missing will work for any
   * method call detected.
   *
   * Method stubs are added to the BasicObject prototype, which every other
   * ruby object inherits, so all objects should handle method missing. A stub
   * is only added if the given property name (method name) is not already
   * defined.
   *
   * Note: all ruby methods have a `$` prefix in javascript, so all stubs will
   * have this prefix as well (to make this method more performant).
   *
   *    Opal.add_stubs(["$foo", "$bar", "$baz="]);
   *
   * All stub functions will have a private `$$stub` property set to true so
   * that other internal methods can detect if a method is just a stub or not.
   * `Kernel#respond_to?` uses this property to detect a methods presence.
   *
   * @param [Array] stubs an array of method stubs to add
   */
  Opal.add_stubs = function(stubs) {
    var subscribers = Opal.stub_subscribers;
    var subscriber;

    for (var i = 0, length = stubs.length; i < length; i++) {
      var method_name = stubs[i], stub = stub_for(method_name);

      for (var j = 0; j < subscribers.length; j++) {
        subscriber = subscribers[j];
        if (!(method_name in subscriber)) {
          subscriber[method_name] = stub;
        }
      }
    }
  };

  /*
   * Add a prototype to the subscribers list, and (TODO) add previously stubbed
   * methods.
   *
   * @param [Prototype]
   */
  function add_stubs_subscriber(prototype) {
    // TODO: Add previously stubbed methods too.
    Opal.stub_subscribers.push(prototype);
  }

  /*
   * Keep a list of prototypes that want method_missing stubs to be added.
   *
   * @default [Prototype List] BasicObject.prototype
   */
  Opal.stub_subscribers = [BasicObject.prototype];

  /*
   * Add a method_missing stub function to the given prototype for the
   * given name.
   *
   * @param [Prototype] prototype the target prototype
   * @param [String] stub stub name to add (e.g. "$foo")
   */
  function add_stub_for(prototype, stub) {
    var method_missing_stub = stub_for(stub);
    prototype[stub] = method_missing_stub;
  }

  /*
   * Generate the method_missing stub for a given method name.
   *
   * @param [String] method_name The js-name of the method to stub (e.g. "$foo")
   */
  function stub_for(method_name) {
    function method_missing_stub() {
      // Copy any given block onto the method_missing dispatcher
      this.$method_missing.$$p = method_missing_stub.$$p;

      // Set block property to null ready for the next call (stop false-positives)
      method_missing_stub.$$p = null;

      // call method missing with correct args (remove '$' prefix on method name)
      return this.$method_missing.apply(this, [method_name.slice(1)].concat($slice.call(arguments)));
    }

    method_missing_stub.$$stub = true;

    return method_missing_stub;
  }

  // Expose for other parts of Opal to use
  Opal.add_stub_for = add_stub_for;

  // Arity count error dispatcher
  Opal.ac = function(actual, expected, object, meth) {
    var inspect = (object.$$is_class ? object.$$name + '.' : object.$$class.$$name + '#') + meth;
    var msg = '[' + inspect + '] wrong number of arguments(' + actual + ' for ' + expected + ')';
    throw Opal.ArgumentError.$new(msg);
  };

  // Super dispatcher
  Opal.find_super_dispatcher = function(obj, jsid, current_func, iter, defs) {
    var dispatcher;

    if (defs) {
      dispatcher = obj.$$is_class ? defs.$$super : obj.$$class.$$proto;
    }
    else {
      if (obj.$$is_class) {
        dispatcher = obj.$$super;
      }
      else {
        dispatcher = find_obj_super_dispatcher(obj, jsid, current_func);
      }
    }

    dispatcher = dispatcher['$' + jsid];
    dispatcher.$$p = iter;

    return dispatcher;
  };

  // Iter dispatcher for super in a block
  Opal.find_iter_super_dispatcher = function(obj, jsid, current_func, iter, defs) {
    if (current_func.$$def) {
      return Opal.find_super_dispatcher(obj, current_func.$$jsid, current_func, iter, defs);
    }
    else {
      return Opal.find_super_dispatcher(obj, jsid, current_func, iter, defs);
    }
  };

  function find_obj_super_dispatcher(obj, jsid, current_func) {
    var klass = obj.$$meta || obj.$$class;
    jsid = '$' + jsid;

    while (klass) {
      if (klass.$$proto[jsid] === current_func) {
        // ok
        break;
      }

      klass = klass.$$parent;
    }

    // if we arent in a class, we couldnt find current?
    if (!klass) {
      throw new Error("could not find current class for super()");
    }

    klass = klass.$$parent;

    // else, let's find the next one
    while (klass) {
      var working = klass.$$proto[jsid];

      if (working && working !== current_func) {
        // ok
        break;
      }

      klass = klass.$$parent;
    }

    return klass.$$proto;
  };

  /*
   * Used to return as an expression. Sometimes, we can't simply return from
   * a javascript function as if we were a method, as the return is used as
   * an expression, or even inside a block which must "return" to the outer
   * method. This helper simply throws an error which is then caught by the
   * method. This approach is expensive, so it is only used when absolutely
   * needed.
   */
  Opal.ret = function(val) {
    Opal.returner.$v = val;
    throw Opal.returner;
  };

  // handles yield calls for 1 yielded arg
  Opal.yield1 = function(block, arg) {
    if (typeof(block) !== "function") {
      throw Opal.LocalJumpError.$new("no block given");
    }

    if (block.length > 1 && arg.$$is_array) {
      return block.apply(null, arg);
    }
    else {
      return block(arg);
    }
  };

  // handles yield for > 1 yielded arg
  Opal.yieldX = function(block, args) {
    if (typeof(block) !== "function") {
      throw Opal.LocalJumpError.$new("no block given");
    }

    if (block.length > 1 && args.length == 1) {
      if (args[0].$$is_array) {
        return block.apply(null, args[0]);
      }
    }

    if (!args.$$is_array) {
      args = $slice.call(args);
    }

    return block.apply(null, args);
  };

  // Finds the corresponding exception match in candidates.  Each candidate can
  // be a value, or an array of values.  Returns null if not found.
  Opal.rescue = function(exception, candidates) {
    for (var i = 0; i < candidates.length; i++) {
      var candidate = candidates[i];

      if (candidate.$$is_array) {
        var result = Opal.rescue(exception, candidate);

        if (result) {
          return result;
        }
      }
      else if (candidate['$==='](exception)) {
        return candidate;
      }
    }

    return null;
  };

  Opal.is_a = function(object, klass) {
    if (object.$$meta === klass) {
      return true;
    }

    var search = object.$$class;

    while (search) {
      if (search === klass) {
        return true;
      }

      for (var i = 0, length = search.$$inc.length; i < length; i++) {
        if (search.$$inc[i] == klass) {
          return true;
        }
      }

      search = search.$$super;
    }

    return false;
  };

  // Helper to convert the given object to an array
  Opal.to_ary = function(value) {
    if (value.$$is_array) {
      return value;
    }
    else if (value.$to_ary && !value.$to_ary.$$stub) {
      return value.$to_ary();
    }

    return [value];
  };

  /**
    Used to get a list of rest keyword arguments. Method takes the given
    keyword args, i.e. the hash literal passed to the method containing all
    keyword arguemnts passed to method, as well as the used args which are
    the names of required and optional arguments defined. This method then
    just returns all key/value pairs which have not been used, in a new
    hash literal.

    @param given_args [Hash] all kwargs given to method
    @param used_args [Object<String: true>] all keys used as named kwargs
    @return [Hash]
   */
  Opal.kwrestargs = function(given_args, used_args) {
    var keys      = [],
        map       = {},
        key       = null,
        given_map = given_args.smap;

    for (key in given_map) {
      if (!used_args[key]) {
        keys.push(key);
        map[key] = given_map[key];
      }
    }

    return Opal.hash2(keys, map);
  };

  /*
   * Call a ruby method on a ruby object with some arguments:
   *
   *   var my_array = [1, 2, 3, 4]
   *   Opal.send(my_array, 'length')     # => 4
   *   Opal.send(my_array, 'reverse!')   # => [4, 3, 2, 1]
   *
   * A missing method will be forwarded to the object via
   * method_missing.
   *
   * The result of either call with be returned.
   *
   * @param [Object] recv the ruby object
   * @param [String] mid ruby method to call
   */
  Opal.send = function(recv, mid) {
    var args = $slice.call(arguments, 2),
        func = recv['$' + mid];

    if (func) {
      return func.apply(recv, args);
    }

    return recv.$method_missing.apply(recv, [mid].concat(args));
  };

  Opal.block_send = function(recv, mid, block) {
    var args = $slice.call(arguments, 3),
        func = recv['$' + mid];

    if (func) {
      func.$$p = block;
      return func.apply(recv, args);
    }

    return recv.$method_missing.apply(recv, [mid].concat(args));
  };

  /*
   * Donate methods for a class/module
   */
  function donate_methods(klass, defined, indirect) {
    var methods = klass.$$methods, included_in = klass.$$dep;

    // if (!indirect) {
      klass.$$methods = methods.concat(defined);
    // }

    if (included_in) {
      for (var i = 0, length = included_in.length; i < length; i++) {
        var includee = included_in[i];
        var dest     = includee.$$proto;

        for (var j = 0, jj = defined.length; j < jj; j++) {
          var method = defined[j];

          dest[method] = klass.$$proto[method];
          dest[method].$$donated = true;
        }

        if (includee.$$dep) {
          donate_methods(includee, defined, true);
        }
      }
    }
  };

  /**
    Define the given method on the module.

    This also handles donating methods to all classes that include this
    module. Method conflicts are also handled here, where a class might already
    have defined a method of the same name, or another included module defined
    the same method.

    @param [RubyModule] module the module method defined on
    @param [String] jsid javascript friendly method name (e.g. "$foo")
    @param [Function] body method body of actual function
  */
  function define_module_method(module, jsid, body) {
    module.$$proto[jsid] = body;
    body.$$owner = module;

    module.$$methods.push(jsid);

    if (module.$$module_function) {
      module[jsid] = body;
    }

    var included_in = module.$$dep;

    if (included_in) {
      for (var i = 0, length = included_in.length; i < length; i++) {
        var includee = included_in[i];
        var dest = includee.$$proto;
        var current = dest[jsid];


        if (dest.hasOwnProperty(jsid) && !current.$$donated && !current.$$stub) {
          // target class has already defined the same method name - do nothing
        }
        else if (dest.hasOwnProperty(jsid) && !current.$$stub) {
          // target class includes another module that has defined this method
          var klass_includees = includee.$$inc;

          for (var j = 0, jj = klass_includees.length; j < jj; j++) {
            if (klass_includees[j] === current.$$owner) {
              var current_owner_index = j;
            }
            if (klass_includees[j] === module) {
              var module_index = j;
            }
          }

          // only redefine method on class if the module was included AFTER
          // the module which defined the current method body. Also make sure
          // a module can overwrite a method it defined before
          if (current_owner_index <= module_index) {
            dest[jsid] = body;
            dest[jsid].$$donated = true;
          }
        }
        else {
          // neither a class, or module included by class, has defined method
          dest[jsid] = body;
          dest[jsid].$$donated = true;
        }

        if (includee.$$dep) {
          donate_methods(includee, [jsid], true);
        }
      }
    }
  }

  /**
    Used to define methods on an object. This is a helper method, used by the
    compiled source to define methods on special case objects when the compiler
    can not determine the destination object, or the object is a Module
    instance. This can get called by `Module#define_method` as well.

    ## Modules

    Any method defined on a module will come through this runtime helper.
    The method is added to the module body, and the owner of the method is
    set to be the module itself. This is used later when choosing which
    method should show on a class if more than 1 included modules define
    the same method. Finally, if the module is in `module_function` mode,
    then the method is also defined onto the module itself.

    ## Classes

    This helper will only be called for classes when a method is being
    defined indirectly; either through `Module#define_method`, or by a
    literal `def` method inside an `instance_eval` or `class_eval` body. In
    either case, the method is simply added to the class' prototype. A special
    exception exists for `BasicObject` and `Object`. These two classes are
    special because they are used in toll-free bridged classes. In each of
    these two cases, extra work is required to define the methods on toll-free
    bridged class' prototypes as well.

    ## Objects

    If a simple ruby object is the object, then the method is simply just
    defined on the object as a singleton method. This would be the case when
    a method is defined inside an `instance_eval` block.

    @param [RubyObject or Class] obj the actual obj to define method for
    @param [String] jsid the javascript friendly method name (e.g. '$foo')
    @param [Function] body the literal javascript function used as method
    @returns [null]
  */
  Opal.defn = function(obj, jsid, body) {
    if (obj.$$is_mod) {
      define_module_method(obj, jsid, body);
    }
    else if (obj.$$is_class) {
      obj.$$proto[jsid] = body;

      if (obj === BasicObjectClass) {
        define_basic_object_method(jsid, body);
      }
      else if (obj === ObjectClass) {
        donate_methods(obj, [jsid]);
      }
    }
    else {
      obj[jsid] = body;
    }

    return nil;
  };

  /*
   * Define a singleton method on the given object.
   */
  Opal.defs = function(obj, jsid, body) {
    if (obj.$$is_class || obj.$$is_mod) {
      obj.constructor.prototype[jsid] = body;
    }
    else {
      obj[jsid] = body;
    }
  };

  function define_basic_object_method(jsid, body) {
    BasicObjectClass.$$methods.push(jsid);
    for (var i = 0, len = bridged_classes.length; i < len; i++) {
      bridged_classes[i].$$proto[jsid] = body;
    }
  }

  Opal.hash = function() {
    if (arguments.length == 1 && arguments[0].$$class == Opal.Hash) {
      return arguments[0];
    }

    var hash = new Opal.Hash.$$alloc(),
        keys = [],
        _map = {},
        smap = {},
        key, obj, length, khash;

    hash.map   = _map;
    hash.smap  = smap;
    hash.keys  = keys;

    if (arguments.length == 1) {
      if (arguments[0].$$is_array) {
        var args = arguments[0];

        for (var i = 0, ii = args.length; i < ii; i++) {
          var pair = args[i];

          if (pair.length !== 2) {
            throw Opal.ArgumentError.$new("value not of length 2: " + pair.$inspect());
          }

          key = pair[0];
          obj = pair[1];

          if (key.$$is_string) {
            khash = key;
            map = smap;
          } else {
            khash = key.$hash();
            map = _map;
          }

          if (map[khash] == null) {
            keys.push(key);
          }

          map[khash] = obj;
        }
      }
      else {
        obj = arguments[0];
        for (key in obj) {
          khash = key.$hash();
          map[khash] = obj[khash];
          keys.push(key);
        }
      }
    }
    else {
      length = arguments.length;
      if (length % 2 !== 0) {
        throw Opal.ArgumentError.$new("odd number of arguments for Hash");
      }

      for (var j = 0; j < length; j++) {
        key = arguments[j];
        obj = arguments[++j];

        if (key.$$is_string) {
          khash = key;
          map = smap;
        } else {
          khash = key.$hash();
          map = _map;
        }

        if (map[khash] == null) {
          keys.push(key);
        }

        map[khash] = obj;
      }
    }

    return hash;
  };

  /*
   * hash2 is a faster creator for hashes that just use symbols and
   * strings as keys. The map and keys array can be constructed at
   * compile time, so they are just added here by the constructor
   * function
   */
  Opal.hash2 = function(keys, map) {
    var hash = new Opal.Hash.$$alloc();

    hash.keys = keys;
    hash.map  = {};
    hash.smap = map;

    return hash;
  };

  /*
   * Create a new range instance with first and last values, and whether the
   * range excludes the last value.
   */
  Opal.range = function(first, last, exc) {
    var range         = new Opal.Range.$$alloc();
        range.begin   = first;
        range.end     = last;
        range.exclude = exc;

    return range;
  };

  // Require system
  // --------------
  (function(Opal) {
    var loaded_features = ['corelib/runtime.js'],
        require_table   = {'corelib/runtime.js': true},
        modules         = {};

    var current_dir  = '.';

    function mark_as_loaded(filename) {
      if (require_table[filename]) {
        return false;
      }

      loaded_features.push(filename);
      require_table[filename] = true;

      return true;
    }

    function normalize_loadable_path(path) {
      var parts, part, new_parts = [], SEPARATOR = '/';

      if (current_dir !== '.') {
        path = current_dir.replace(/\/*$/, '/') + path;
      }

      parts = path.split(SEPARATOR);

      for (var i = 0, ii = parts.length; i < ii; i++) {
        part = parts[i];
        if (part == '') continue;
        (part === '..') ? new_parts.pop() : new_parts.push(part)
      }

      return new_parts.join(SEPARATOR);
    }

    function load(path) {
      mark_as_loaded(path);

      var module = modules[path];

      if (module) {
        module(Opal);
      }
      else {
        var severity = Opal.dynamic_require_severity || 'warning';
        var message  = 'cannot load such file -- ' + path;

        if (severity === "error") {
          Opal.LoadError ? Opal.LoadError.$new(message) : function(){throw message}();
        }
        else if (severity === "warning") {
          console.warn('WARNING: LoadError: ' + message);
        }
      }

      return true;
    }

    function require(path) {
      if (require_table[path]) {
        return false;
      }

      return load(path);
    }

    Opal.modules         = modules;
    Opal.loaded_features = loaded_features;

    Opal.normalize_loadable_path = normalize_loadable_path;
    Opal.mark_as_loaded          = mark_as_loaded;

    Opal.load    = load;
    Opal.require = require;
  })(Opal);

  // Initialization
  // --------------

  // The actual class for BasicObject
  var BasicObjectClass;

  // The actual Object class
  var ObjectClass;

  // The actual Module class
  var ModuleClass;

  // The actual Class class
  var ClassClass;

  // Constructor for instances of BasicObject
  function BasicObject(){}

  // Constructor for instances of Object
  function Object(){}

  // Constructor for instances of Class
  function Class(){}

  // Constructor for instances of Module
  function Module(){}

  // Constructor for instances of NilClass (nil)
  function NilClass(){}

  // Constructors for *instances* of core objects
  boot_class_alloc('BasicObject', BasicObject);
  boot_class_alloc('Object',      Object,       BasicObject);
  boot_class_alloc('Module',      Module,       Object);
  boot_class_alloc('Class',       Class,        Module);

  // Constructors for *classes* of core objects
  BasicObjectClass = boot_core_class_object('BasicObject', BasicObject, Class);
  ObjectClass      = boot_core_class_object('Object',      Object,      BasicObjectClass.constructor);
  ModuleClass      = boot_core_class_object('Module',      Module,      ObjectClass.constructor);
  ClassClass       = boot_core_class_object('Class',       Class,       ModuleClass.constructor);

  // Fix booted classes to use their metaclass
  BasicObjectClass.$$class = ClassClass;
  ObjectClass.$$class      = ClassClass;
  ModuleClass.$$class      = ClassClass;
  ClassClass.$$class       = ClassClass;

  // Fix superclasses of booted classes
  BasicObjectClass.$$super = null;
  ObjectClass.$$super      = BasicObjectClass;
  ModuleClass.$$super      = ObjectClass;
  ClassClass.$$super       = ModuleClass;

  BasicObjectClass.$$parent = null;
  ObjectClass.$$parent      = BasicObjectClass;
  ModuleClass.$$parent      = ObjectClass;
  ClassClass.$$parent       = ModuleClass;

  // Internally, Object acts like a module as it is "included" into bridged
  // classes. In other words, we donate methods from Object into our bridged
  // classes as their prototypes don't inherit from our root Object, so they
  // act like module includes.
  ObjectClass.$$dep = bridged_classes;

  Opal.base                     = ObjectClass;
  BasicObjectClass.$$scope      = ObjectClass.$$scope = Opal;
  BasicObjectClass.$$orig_scope = ObjectClass.$$orig_scope = Opal;
  Opal.Kernel                   = ObjectClass;

  ModuleClass.$$scope      = ObjectClass.$$scope;
  ModuleClass.$$orig_scope = ObjectClass.$$orig_scope;
  ClassClass.$$scope       = ObjectClass.$$scope;
  ClassClass.$$orig_scope  = ObjectClass.$$orig_scope;

  ObjectClass.$$proto.toString = function() {
    return this.$to_s();
  };

  ObjectClass.$$proto.$require = Opal.require;

  Opal.top = new ObjectClass.$$alloc();

  // Nil
  var nil_id = Opal.uid(); // nil id is traditionally 4
  Opal.klass(ObjectClass, ObjectClass, 'NilClass', NilClass);
  var nil = Opal.nil = new NilClass();
  nil.$$id = nil_id;
  nil.call = nil.apply = function() { throw Opal.LocalJumpError.$new('no block given'); };

  Opal.breaker  = new Error('unexpected break');
  Opal.returner = new Error('unexpected return');

  bridge_class('Array',     Array);
  bridge_class('Boolean',   Boolean);
  bridge_class('Numeric',   Number);
  bridge_class('String',    String);
  bridge_class('Proc',      Function);
  bridge_class('Exception', Error);
  bridge_class('Regexp',    RegExp);
  bridge_class('Time',      Date);

  TypeError.$$super = Error;
}).call(this);

if (typeof(global) !== 'undefined') {
  global.Opal = this.Opal;
  Opal.global = global;
}
if (typeof(window) !== 'undefined') {
  window.Opal = this.Opal;
  Opal.global = window;
}
Opal.mark_as_loaded(Opal.normalize_loadable_path("corelib/runtime"));
/* Generated by Opal 0.7.2 */
Opal.modules["corelib/helpers"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module;

  Opal.add_stubs(['$new', '$class', '$===', '$respond_to?', '$raise', '$type_error', '$__send__', '$coerce_to', '$nil?', '$<=>', '$inspect']);
  return (function($base) {
    var self = $module($base, 'Opal');

    var def = self.$$proto, $scope = self.$$scope;

    Opal.defs(self, '$type_error', function(object, type, method, coerced) {
      var $a, $b, self = this;

      if (method == null) {
        method = nil
      }
      if (coerced == null) {
        coerced = nil
      }
      if ((($a = (($b = method !== false && method !== nil) ? coerced : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return $scope.get('TypeError').$new("can't convert " + (object.$class()) + " into " + (type) + " (" + (object.$class()) + "#" + (method) + " gives " + (coerced.$class()))
        } else {
        return $scope.get('TypeError').$new("no implicit conversion of " + (object.$class()) + " into " + (type))
      };
    });

    Opal.defs(self, '$coerce_to', function(object, type, method) {
      var $a, self = this;

      if ((($a = type['$==='](object)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return object};
      if ((($a = object['$respond_to?'](method)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise(self.$type_error(object, type))
      };
      return object.$__send__(method);
    });

    Opal.defs(self, '$coerce_to!', function(object, type, method) {
      var $a, self = this, coerced = nil;

      coerced = self.$coerce_to(object, type, method);
      if ((($a = type['$==='](coerced)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise(self.$type_error(object, type, method, coerced))
      };
      return coerced;
    });

    Opal.defs(self, '$coerce_to?', function(object, type, method) {
      var $a, self = this, coerced = nil;

      if ((($a = object['$respond_to?'](method)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        return nil
      };
      coerced = self.$coerce_to(object, type, method);
      if ((($a = coerced['$nil?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
        return nil};
      if ((($a = type['$==='](coerced)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise(self.$type_error(object, type, method, coerced))
      };
      return coerced;
    });

    Opal.defs(self, '$try_convert', function(object, type, method) {
      var $a, self = this;

      if ((($a = type['$==='](object)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return object};
      if ((($a = object['$respond_to?'](method)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return object.$__send__(method)
        } else {
        return nil
      };
    });

    Opal.defs(self, '$compare', function(a, b) {
      var $a, self = this, compare = nil;

      compare = a['$<=>'](b);
      if ((($a = compare === nil) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('ArgumentError'), "comparison of " + (a.$class()) + " with " + (b.$class()) + " failed")};
      return compare;
    });

    Opal.defs(self, '$destructure', function(args) {
      var self = this;

      
      if (args.length == 1) {
        return args[0];
      }
      else if (args.$$is_array) {
        return args;
      }
      else {
        return $slice.call(args);
      }
    
    });

    Opal.defs(self, '$respond_to?', function(obj, method) {
      var self = this;

      
      if (obj == null || !obj.$$class) {
        return false;
      }
    
      return obj['$respond_to?'](method);
    });

    Opal.defs(self, '$inspect', function(obj) {
      var self = this;

      
      if (obj === undefined) {
        return "undefined";
      }
      else if (obj === null) {
        return "null";
      }
      else if (!obj.$$class) {
        return obj.toString();
      }
      else {
        return obj.$inspect();
      }
    
    });
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["corelib/module"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  Opal.add_stubs(['$attr_reader', '$attr_writer', '$coerce_to!', '$raise', '$=~', '$const_missing', '$const_get', '$to_str', '$to_proc', '$append_features', '$included', '$name', '$new', '$to_s', '$__id__']);
  return (function($base, $super) {
    function $Module(){};
    var self = $Module = $klass($base, $super, 'Module', $Module);

    var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_2, TMP_3, TMP_4;

    Opal.defs(self, '$new', TMP_1 = function() {
      var self = this, $iter = TMP_1.$$p, block = $iter || nil;

      TMP_1.$$p = null;
      
      function AnonModule(){}
      var klass      = Opal.boot(Opal.Module, AnonModule);
      klass.$$name   = nil;
      klass.$$class  = Opal.Module;
      klass.$$dep    = []
      klass.$$is_mod = true;
      klass.$$proto  = {};

      // inherit scope from parent
      Opal.create_scope(Opal.Module.$$scope, klass);

      if (block !== nil) {
        var block_self = block.$$s;
        block.$$s = null;
        block.call(klass);
        block.$$s = block_self;
      }

      return klass;
    
    });

    def['$==='] = function(object) {
      var $a, self = this;

      if ((($a = object == null) !== nil && (!$a.$$is_boolean || $a == true))) {
        return false};
      return Opal.is_a(object, self);
    };

    def['$<'] = function(other) {
      var self = this;

      
      var working = self;

      while (working) {
        if (working === other) {
          return true;
        }

        working = working.$$parent;
      }

      return false;
    
    };

    def.$alias_method = function(newname, oldname) {
      var self = this;

      
      var newjsid = '$' + newname,
          body    = self.$$proto['$' + oldname];

      if (self.$$is_singleton) {
        self.$$proto[newjsid] = body;
      }
      else {
        Opal.defn(self, newjsid, body);
      }

      return self;
    
      return self;
    };

    def.$alias_native = function(mid, jsid) {
      var self = this;

      if (jsid == null) {
        jsid = mid
      }
      return self.$$proto['$' + mid] = self.$$proto[jsid];
    };

    def.$ancestors = function() {
      var self = this;

      
      var parent = self,
          result = [];

      while (parent) {
        result.push(parent);
        result = result.concat(parent.$$inc);

        parent = parent.$$super;
      }

      return result;
    
    };

    def.$append_features = function(klass) {
      var self = this;

      Opal.append_features(self, klass);
      return self;
    };

    def.$attr_accessor = function(names) {
      var $a, $b, self = this;

      names = $slice.call(arguments, 0);
      ($a = self).$attr_reader.apply($a, [].concat(names));
      return ($b = self).$attr_writer.apply($b, [].concat(names));
    };

    def.$attr_reader = function(names) {
      var self = this;

      names = $slice.call(arguments, 0);
      
      for (var i = 0, length = names.length; i < length; i++) {
        (function(name) {
          self.$$proto[name] = nil;
          var func = function() { return this[name] };

          if (self.$$is_singleton) {
            self.$$proto.constructor.prototype['$' + name] = func;
          }
          else {
            Opal.defn(self, '$' + name, func);
          }
        })(names[i]);
      }
    
      return nil;
    };

    def.$attr_writer = function(names) {
      var self = this;

      names = $slice.call(arguments, 0);
      
      for (var i = 0, length = names.length; i < length; i++) {
        (function(name) {
          self.$$proto[name] = nil;
          var func = function(value) { return this[name] = value; };

          if (self.$$is_singleton) {
            self.$$proto.constructor.prototype['$' + name + '='] = func;
          }
          else {
            Opal.defn(self, '$' + name + '=', func);
          }
        })(names[i]);
      }
    
      return nil;
    };

    Opal.defn(self, '$attr', def.$attr_accessor);

    def.$autoload = function(const$, path) {
      var self = this;

      
      var autoloaders;

      if (!(autoloaders = self.$$autoload)) {
        autoloaders = self.$$autoload = {};
      }

      autoloaders[const$] = path;
      return nil;
    ;
    };

    def.$class_variable_get = function(name) {
      var $a, self = this;

      name = $scope.get('Opal')['$coerce_to!'](name, $scope.get('String'), "to_str");
      if ((($a = name.length < 3 || name.slice(0,2) !== '@@') !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('NameError'), "class vars should start with @@")};
      
      var value = Opal.cvars[name.slice(2)];
      (function() {if ((($a = value == null) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self.$raise($scope.get('NameError'), "uninitialized class variable @@a in")
        } else {
        return nil
      }; return nil; })()
      return value;
    
    };

    def.$class_variable_set = function(name, value) {
      var $a, self = this;

      name = $scope.get('Opal')['$coerce_to!'](name, $scope.get('String'), "to_str");
      if ((($a = name.length < 3 || name.slice(0,2) !== '@@') !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('NameError'))};
      
      Opal.cvars[name.slice(2)] = value;
      return value;
    
    };

    def.$constants = function() {
      var self = this;

      return self.$$scope.constants;
    };

    def['$const_defined?'] = function(name, inherit) {
      var $a, self = this;

      if (inherit == null) {
        inherit = true
      }
      if ((($a = name['$=~'](/^[A-Z]\w*$/)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise($scope.get('NameError'), "wrong constant name " + (name))
      };
      
      scopes = [self.$$scope];
      if (inherit || self === Opal.Object) {
        var parent = self.$$super;
        while (parent !== Opal.BasicObject) {
          scopes.push(parent.$$scope);
          parent = parent.$$super;
        }
      }

      for (var i = 0, len = scopes.length; i < len; i++) {
        if (scopes[i].hasOwnProperty(name)) {
          return true;
        }
      }

      return false;
    
    };

    def.$const_get = function(name, inherit) {
      var $a, self = this;

      if (inherit == null) {
        inherit = true
      }
      if ((($a = name['$=~'](/^[A-Z]\w*$/)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise($scope.get('NameError'), "wrong constant name " + (name))
      };
      
      var scopes = [self.$$scope];
      if (inherit || self == Opal.Object) {
        var parent = self.$$super;
        while (parent !== Opal.BasicObject) {
          scopes.push(parent.$$scope);
          parent = parent.$$super;
        }
      }

      for (var i = 0, len = scopes.length; i < len; i++) {
        if (scopes[i].hasOwnProperty(name)) {
          return scopes[i][name];
        }
      }

      return self.$const_missing(name);
    
    };

    def.$const_missing = function(const$) {
      var self = this;

      
      if (self.$$autoload) {
        var file = self.$$autoload[const$];

        if (file) {
          self.$require(file);

          return self.$const_get(const$);
        }
      }
    ;
      return self.$raise($scope.get('NameError'), "uninitialized constant " + (self) + "::" + (const$));
    };

    def.$const_set = function(name, value) {
      var $a, self = this;

      if ((($a = name['$=~'](/^[A-Z]\w*$/)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise($scope.get('NameError'), "wrong constant name " + (name))
      };
      try {
      name = name.$to_str()
      } catch ($err) {if (true) {
        self.$raise($scope.get('TypeError'), "conversion with #to_str failed")
        }else { throw $err; }
      };
      Opal.casgn(self, name, value);
      return value;
    };

    def.$define_method = TMP_2 = function(name, method) {
      var self = this, $iter = TMP_2.$$p, block = $iter || nil;

      TMP_2.$$p = null;
      
      if (method) {
        block = method.$to_proc();
      }

      if (block === nil) {
        throw new Error("no block given");
      }

      var jsid    = '$' + name;
      block.$$jsid = name;
      block.$$s    = null;
      block.$$def  = block;

      if (self.$$is_singleton) {
        self.$$proto[jsid] = block;
      }
      else {
        Opal.defn(self, jsid, block);
      }

      return name;
    ;
    };

    def.$remove_method = function(name) {
      var self = this;

      
      var jsid    = '$' + name;
      var current = self.$$proto[jsid];
      delete self.$$proto[jsid];

      // Check if we need to reverse Opal.donate
      // Opal.retire(self, [jsid]);
      return self;
    
    };

    def.$include = function(mods) {
      var self = this;

      mods = $slice.call(arguments, 0);
      
      for (var i = mods.length - 1; i >= 0; i--) {
        var mod = mods[i];

        if (mod === self) {
          continue;
        }

        (mod).$append_features(self);
        (mod).$included(self);
      }
    
      return self;
    };

    def['$include?'] = function(mod) {
      var self = this;

      
      for (var cls = self; cls; cls = cls.parent) {
        for (var i = 0; i != cls.$$inc.length; i++) {
          var mod2 = cls.$$inc[i];
          if (mod === mod2) {
            return true;
          }
        }
      }
      return false;
    
    };

    def.$instance_method = function(name) {
      var self = this;

      
      var meth = self.$$proto['$' + name];

      if (!meth || meth.$$stub) {
        self.$raise($scope.get('NameError'), "undefined method `" + (name) + "' for class `" + (self.$name()) + "'");
      }

      return $scope.get('UnboundMethod').$new(self, meth, name);
    
    };

    def.$instance_methods = function(include_super) {
      var self = this;

      if (include_super == null) {
        include_super = false
      }
      
      var methods = [],
          proto   = self.$$proto;

      for (var prop in proto) {
        if (!prop.charAt(0) === '$') {
          continue;
        }

        if (typeof(proto[prop]) !== "function") {
          continue;
        }

        if (proto[prop].$$stub) {
          continue;
        }

        if (!self.$$is_mod) {
          if (self !== Opal.BasicObject && proto[prop] === Opal.BasicObject.$$proto[prop]) {
            continue;
          }

          if (!include_super && !proto.hasOwnProperty(prop)) {
            continue;
          }

          if (!include_super && proto[prop].$$donated) {
            continue;
          }
        }

        methods.push(prop.substr(1));
      }

      return methods;
    
    };

    def.$included = function(mod) {
      var self = this;

      return nil;
    };

    def.$extended = function(mod) {
      var self = this;

      return nil;
    };

    def.$module_eval = TMP_3 = function() {
      var self = this, $iter = TMP_3.$$p, block = $iter || nil;

      TMP_3.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        self.$raise($scope.get('ArgumentError'), "no block given")
      };
      
      var old = block.$$s,
          result;

      block.$$s = null;
      result = block.call(self);
      block.$$s = old;

      return result;
    
    };

    Opal.defn(self, '$class_eval', def.$module_eval);

    def.$module_exec = TMP_4 = function() {
      var self = this, $iter = TMP_4.$$p, block = $iter || nil;

      TMP_4.$$p = null;
      
      if (block === nil) {
        throw new Error("no block given");
      }

      var block_self = block.$$s, result;

      block.$$s = null;
      result = block.apply(self, $slice.call(arguments));
      block.$$s = block_self;

      return result;
    
    };

    Opal.defn(self, '$class_exec', def.$module_exec);

    def['$method_defined?'] = function(method) {
      var self = this;

      
      var body = self.$$proto['$' + method];
      return (!!body) && !body.$$stub;
    
    };

    def.$module_function = function(methods) {
      var self = this;

      methods = $slice.call(arguments, 0);
      
      if (methods.length === 0) {
        self.$$module_function = true;
      }
      else {
        for (var i = 0, length = methods.length; i < length; i++) {
          var meth = methods[i], func = self.$$proto['$' + meth];

          self.constructor.prototype['$' + meth] = func;
        }
      }

      return self;
    
    };

    def.$name = function() {
      var self = this;

      
      if (self.$$full_name) {
        return self.$$full_name;
      }

      var result = [], base = self;

      while (base) {
        if (base.$$name === nil) {
          return result.length === 0 ? nil : result.join('::');
        }

        result.unshift(base.$$name);

        base = base.$$base_module;

        if (base === Opal.Object) {
          break;
        }
      }

      if (result.length === 0) {
        return nil;
      }

      return self.$$full_name = result.join('::');
    
    };

    def.$public = function(methods) {
      var self = this;

      methods = $slice.call(arguments, 0);
      
      if (methods.length === 0) {
        self.$$module_function = false;
      }

      return nil;
    
    };

    Opal.defn(self, '$private', def.$public);

    Opal.defn(self, '$protected', def.$public);

    Opal.defn(self, '$nesting', def.$public);

    def.$private_class_method = function(name) {
      var self = this;

      return self['$' + name] || nil;
    };

    Opal.defn(self, '$public_class_method', def.$private_class_method);

    def['$private_method_defined?'] = function(obj) {
      var self = this;

      return false;
    };

    def.$private_constant = function() {
      var self = this;

      return nil;
    };

    Opal.defn(self, '$protected_method_defined?', def['$private_method_defined?']);

    Opal.defn(self, '$public_instance_methods', def.$instance_methods);

    Opal.defn(self, '$public_method_defined?', def['$method_defined?']);

    def.$remove_class_variable = function() {
      var self = this;

      return nil;
    };

    def.$remove_const = function(name) {
      var self = this;

      
      var old = self.$$scope[name];
      delete self.$$scope[name];
      return old;
    
    };

    def.$to_s = function() {
      var $a, self = this;

      return ((($a = self.$name()) !== false && $a !== nil) ? $a : "#<" + (self.$$is_mod ? 'Module' : 'Class') + ":0x" + (self.$__id__().$to_s(16)) + ">");
    };

    return (def.$undef_method = function(symbol) {
      var self = this;

      Opal.add_stub_for(self.$$proto, "$" + symbol);
      return self;
    }, nil) && 'undef_method';
  })(self, null)
};

/* Generated by Opal 0.7.2 */
Opal.modules["corelib/class"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  Opal.add_stubs(['$require', '$raise', '$allocate']);
  self.$require("corelib/module");
  return (function($base, $super) {
    function $Class(){};
    var self = $Class = $klass($base, $super, 'Class', $Class);

    var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_2;

    Opal.defs(self, '$new', TMP_1 = function(sup) {
      var self = this, $iter = TMP_1.$$p, block = $iter || nil;

      if (sup == null) {
        sup = $scope.get('Object')
      }
      TMP_1.$$p = null;
      
      if (!sup.$$is_class || sup.$$is_mod) {
        self.$raise($scope.get('TypeError'), "superclass must be a Class");
      }

      function AnonClass(){};
      var klass      = Opal.boot(sup, AnonClass)
      klass.$$name   = nil;
      klass.$$parent = sup;

      // inherit scope from parent
      Opal.create_scope(sup.$$scope, klass);

      sup.$inherited(klass);

      if (block !== nil) {
        var block_self = block.$$s;
        block.$$s = null;
        block.call(klass);
        block.$$s = block_self;
      }

      return klass;
    ;
    });

    def.$allocate = function() {
      var self = this;

      
      var obj = new self.$$alloc;
      obj.$$id = Opal.uid();
      return obj;
    
    };

    def.$inherited = function(cls) {
      var self = this;

      return nil;
    };

    def.$new = TMP_2 = function(args) {
      var self = this, $iter = TMP_2.$$p, block = $iter || nil;

      args = $slice.call(arguments, 0);
      TMP_2.$$p = null;
      
      var obj = self.$allocate();

      obj.$initialize.$$p = block;
      obj.$initialize.apply(obj, args);
      return obj;
    ;
    };

    return (def.$superclass = function() {
      var self = this;

      return self.$$super || nil;
    }, nil) && 'superclass';
  })(self, null);
};

/* Generated by Opal 0.7.2 */
Opal.modules["corelib/basic_object"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  Opal.add_stubs(['$raise', '$inspect']);
  return (function($base, $super) {
    function $BasicObject(){};
    var self = $BasicObject = $klass($base, $super, 'BasicObject', $BasicObject);

    var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_2, TMP_3, TMP_4;

    Opal.defn(self, '$initialize', function() {
      var self = this;

      return nil;
    });

    Opal.defn(self, '$==', function(other) {
      var self = this;

      return self === other;
    });

    Opal.defn(self, '$__id__', function() {
      var self = this;

      return self.$$id || (self.$$id = Opal.uid());
    });

    Opal.defn(self, '$__send__', TMP_1 = function(symbol, args) {
      var self = this, $iter = TMP_1.$$p, block = $iter || nil;

      args = $slice.call(arguments, 1);
      TMP_1.$$p = null;
      
      var func = self['$' + symbol]

      if (func) {
        if (block !== nil) {
          func.$$p = block;
        }

        return func.apply(self, args);
      }

      if (block !== nil) {
        self.$method_missing.$$p = block;
      }

      return self.$method_missing.apply(self, [symbol].concat(args));
    
    });

    Opal.defn(self, '$!', function() {
      var self = this;

      return false;
    });

    Opal.defn(self, '$eql?', def['$==']);

    Opal.defn(self, '$equal?', def['$==']);

    Opal.defn(self, '$instance_eval', TMP_2 = function() {
      var self = this, $iter = TMP_2.$$p, block = $iter || nil;

      TMP_2.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        $scope.get('Kernel').$raise($scope.get('ArgumentError'), "no block given")
      };
      
      var old = block.$$s,
          result;

      block.$$s = null;
      result = block.call(self, self);
      block.$$s = old;

      return result;
    
    });

    Opal.defn(self, '$instance_exec', TMP_3 = function(args) {
      var self = this, $iter = TMP_3.$$p, block = $iter || nil;

      args = $slice.call(arguments, 0);
      TMP_3.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        $scope.get('Kernel').$raise($scope.get('ArgumentError'), "no block given")
      };
      
      var block_self = block.$$s,
          result;

      block.$$s = null;
      result = block.apply(self, args);
      block.$$s = block_self;

      return result;
    
    });

    return (Opal.defn(self, '$method_missing', TMP_4 = function(symbol, args) {
      var $a, self = this, $iter = TMP_4.$$p, block = $iter || nil;

      args = $slice.call(arguments, 1);
      TMP_4.$$p = null;
      return $scope.get('Kernel').$raise($scope.get('NoMethodError'), (function() {if ((($a = self.$inspect && !self.$inspect.$$stub) !== nil && (!$a.$$is_boolean || $a == true))) {
        return "undefined method `" + (symbol) + "' for " + (self.$inspect()) + ":" + (self.$$class)
        } else {
        return "undefined method `" + (symbol) + "' for " + (self.$$class)
      }; return nil; })());
    }), nil) && 'method_missing';
  })(self, null)
};

/* Generated by Opal 0.7.2 */
Opal.modules["corelib/kernel"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $gvars = Opal.gvars;

  Opal.add_stubs(['$raise', '$inspect', '$==', '$class', '$new', '$respond_to?', '$to_ary', '$to_a', '$<<', '$allocate', '$copy_instance_variables', '$initialize_clone', '$initialize_copy', '$singleton_class', '$initialize_dup', '$for', '$to_proc', '$each', '$reverse', '$append_features', '$extended', '$to_i', '$to_s', '$to_f', '$*', '$__id__', '$===', '$empty?', '$ArgumentError', '$nan?', '$infinite?', '$to_int', '$coerce_to!', '$>', '$length', '$print', '$format', '$puts', '$<=', '$[]', '$nil?', '$is_a?', '$rand', '$coerce_to', '$respond_to_missing?', '$try_convert!', '$expand_path', '$join', '$start_with?']);
  return (function($base) {
    var self = $module($base, 'Kernel');

    var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_2, TMP_3, TMP_4, TMP_5, TMP_6, TMP_7, TMP_8, TMP_10;

    Opal.defn(self, '$method_missing', TMP_1 = function(symbol, args) {
      var self = this, $iter = TMP_1.$$p, block = $iter || nil;

      args = $slice.call(arguments, 1);
      TMP_1.$$p = null;
      return self.$raise($scope.get('NoMethodError'), "undefined method `" + (symbol) + "' for " + (self.$inspect()));
    });

    Opal.defn(self, '$=~', function(obj) {
      var self = this;

      return false;
    });

    Opal.defn(self, '$===', function(other) {
      var self = this;

      return self['$=='](other);
    });

    Opal.defn(self, '$<=>', function(other) {
      var self = this;

      
      if (self['$=='](other)) {
        return 0;
      }

      return nil;
    ;
    });

    Opal.defn(self, '$method', function(name) {
      var self = this;

      
      var meth = self['$' + name];

      if (!meth || meth.$$stub) {
        self.$raise($scope.get('NameError'), "undefined method `" + (name) + "' for class `" + (self.$class()) + "'");
      }

      return $scope.get('Method').$new(self, meth, name);
    
    });

    Opal.defn(self, '$methods', function(all) {
      var self = this;

      if (all == null) {
        all = true
      }
      
      var methods = [];

      for (var key in self) {
        if (key[0] == "$" && typeof(self[key]) === "function") {
          if (all == false || all === nil) {
            if (!Opal.hasOwnProperty.call(self, key)) {
              continue;
            }
          }
          if (self[key].$$stub === undefined) {
            methods.push(key.substr(1));
          }
        }
      }

      return methods;
    
    });

    Opal.defn(self, '$Array', TMP_2 = function(object, args) {
      var self = this, $iter = TMP_2.$$p, block = $iter || nil;

      args = $slice.call(arguments, 1);
      TMP_2.$$p = null;
      
      if (object == null || object === nil) {
        return [];
      }
      else if (object['$respond_to?']("to_ary")) {
        return object.$to_ary();
      }
      else if (object['$respond_to?']("to_a")) {
        return object.$to_a();
      }
      else {
        return [object];
      }
    ;
    });

    Opal.defn(self, '$at_exit', TMP_3 = function() {
      var $a, self = this, $iter = TMP_3.$$p, block = $iter || nil;
      if ($gvars.__at_exit__ == null) $gvars.__at_exit__ = nil;

      TMP_3.$$p = null;
      ((($a = $gvars.__at_exit__) !== false && $a !== nil) ? $a : $gvars.__at_exit__ = []);
      return $gvars.__at_exit__['$<<'](block);
    });

    Opal.defn(self, '$caller', function() {
      var self = this;

      return [];
    });

    Opal.defn(self, '$class', function() {
      var self = this;

      return self.$$class;
    });

    Opal.defn(self, '$copy_instance_variables', function(other) {
      var self = this;

      
      for (var name in other) {
        if (name.charAt(0) !== '$') {
          self[name] = other[name];
        }
      }
    
    });

    Opal.defn(self, '$clone', function() {
      var self = this, copy = nil;

      copy = self.$class().$allocate();
      copy.$copy_instance_variables(self);
      copy.$initialize_clone(self);
      return copy;
    });

    Opal.defn(self, '$initialize_clone', function(other) {
      var self = this;

      return self.$initialize_copy(other);
    });

    Opal.defn(self, '$define_singleton_method', TMP_4 = function(name) {
      var self = this, $iter = TMP_4.$$p, body = $iter || nil;

      TMP_4.$$p = null;
      if (body !== false && body !== nil) {
        } else {
        self.$raise($scope.get('ArgumentError'), "tried to create Proc object without a block")
      };
      
      var jsid   = '$' + name;
      body.$$jsid = name;
      body.$$s    = null;
      body.$$def  = body;

      self.$singleton_class().$$proto[jsid] = body;

      return self;
    
    });

    Opal.defn(self, '$dup', function() {
      var self = this, copy = nil;

      copy = self.$class().$allocate();
      copy.$copy_instance_variables(self);
      copy.$initialize_dup(self);
      return copy;
    });

    Opal.defn(self, '$initialize_dup', function(other) {
      var self = this;

      return self.$initialize_copy(other);
    });

    Opal.defn(self, '$enum_for', TMP_5 = function(method, args) {
      var $a, $b, self = this, $iter = TMP_5.$$p, block = $iter || nil;

      args = $slice.call(arguments, 1);
      if (method == null) {
        method = "each"
      }
      TMP_5.$$p = null;
      return ($a = ($b = $scope.get('Enumerator')).$for, $a.$$p = block.$to_proc(), $a).apply($b, [self, method].concat(args));
    });

    Opal.defn(self, '$to_enum', def.$enum_for);

    Opal.defn(self, '$equal?', function(other) {
      var self = this;

      return self === other;
    });

    Opal.defn(self, '$exit', function(status) {
      var $a, $b, self = this;
      if ($gvars.__at_exit__ == null) $gvars.__at_exit__ = nil;

      if (status == null) {
        status = true
      }
      if ((($a = $gvars.__at_exit__) !== nil && (!$a.$$is_boolean || $a == true))) {
        ($a = ($b = $gvars.__at_exit__.$reverse()).$each, $a.$$p = "call".$to_proc(), $a).call($b)};
      if ((($a = status === true) !== nil && (!$a.$$is_boolean || $a == true))) {
        status = 0};
      Opal.exit(status);
      return nil;
    });

    Opal.defn(self, '$extend', function(mods) {
      var self = this;

      mods = $slice.call(arguments, 0);
      
      var singleton = self.$singleton_class();

      for (var i = mods.length - 1; i >= 0; i--) {
        var mod = mods[i];

        (mod).$append_features(singleton);
        (mod).$extended(self);
      }
    ;
      return self;
    });

    Opal.defn(self, '$format', function(format, args) {
      var self = this;

      args = $slice.call(arguments, 1);
      
      var idx = 0;
      return format.replace(/%(\d+\$)?([-+ 0]*)(\d*|\*(\d+\$)?)(?:\.(\d*|\*(\d+\$)?))?([cspdiubBoxXfgeEG])|(%%)/g, function(str, idx_str, flags, width_str, w_idx_str, prec_str, p_idx_str, spec, escaped) {
        if (escaped) {
          return '%';
        }

        var width,
        prec,
        is_integer_spec = ("diubBoxX".indexOf(spec) != -1),
        is_float_spec = ("eEfgG".indexOf(spec) != -1),
        prefix = '',
        obj;

        if (width_str === undefined) {
          width = undefined;
        } else if (width_str.charAt(0) == '*') {
          var w_idx = idx++;
          if (w_idx_str) {
            w_idx = parseInt(w_idx_str, 10) - 1;
          }
          width = (args[w_idx]).$to_i();
        } else {
          width = parseInt(width_str, 10);
        }
        if (!prec_str) {
          prec = is_float_spec ? 6 : undefined;
        } else if (prec_str.charAt(0) == '*') {
          var p_idx = idx++;
          if (p_idx_str) {
            p_idx = parseInt(p_idx_str, 10) - 1;
          }
          prec = (args[p_idx]).$to_i();
        } else {
          prec = parseInt(prec_str, 10);
        }
        if (idx_str) {
          idx = parseInt(idx_str, 10) - 1;
        }
        switch (spec) {
        case 'c':
          obj = args[idx];
          if (obj.$$is_string) {
            str = obj.charAt(0);
          } else {
            str = String.fromCharCode((obj).$to_i());
          }
          break;
        case 's':
          str = (args[idx]).$to_s();
          if (prec !== undefined) {
            str = str.substr(0, prec);
          }
          break;
        case 'p':
          str = (args[idx]).$inspect();
          if (prec !== undefined) {
            str = str.substr(0, prec);
          }
          break;
        case 'd':
        case 'i':
        case 'u':
          str = (args[idx]).$to_i().toString();
          break;
        case 'b':
        case 'B':
          str = (args[idx]).$to_i().toString(2);
          break;
        case 'o':
          str = (args[idx]).$to_i().toString(8);
          break;
        case 'x':
        case 'X':
          str = (args[idx]).$to_i().toString(16);
          break;
        case 'e':
        case 'E':
          str = (args[idx]).$to_f().toExponential(prec);
          break;
        case 'f':
          str = (args[idx]).$to_f().toFixed(prec);
          break;
        case 'g':
        case 'G':
          str = (args[idx]).$to_f().toPrecision(prec);
          break;
        }
        idx++;
        if (is_integer_spec || is_float_spec) {
          if (str.charAt(0) == '-') {
            prefix = '-';
            str = str.substr(1);
          } else {
            if (flags.indexOf('+') != -1) {
              prefix = '+';
            } else if (flags.indexOf(' ') != -1) {
              prefix = ' ';
            }
          }
        }
        if (is_integer_spec && prec !== undefined) {
          if (str.length < prec) {
            str = "0"['$*'](prec - str.length) + str;
          }
        }
        var total_len = prefix.length + str.length;
        if (width !== undefined && total_len < width) {
          if (flags.indexOf('-') != -1) {
            str = str + " "['$*'](width - total_len);
          } else {
            var pad_char = ' ';
            if (flags.indexOf('0') != -1) {
              str = "0"['$*'](width - total_len) + str;
            } else {
              prefix = " "['$*'](width - total_len) + prefix;
            }
          }
        }
        var result = prefix + str;
        if ('XEG'.indexOf(spec) != -1) {
          result = result.toUpperCase();
        }
        return result;
      });
    
    });

    Opal.defn(self, '$freeze', function() {
      var self = this;

      self.___frozen___ = true;
      return self;
    });

    Opal.defn(self, '$frozen?', function() {
      var $a, self = this;
      if (self.___frozen___ == null) self.___frozen___ = nil;

      return ((($a = self.___frozen___) !== false && $a !== nil) ? $a : false);
    });

    Opal.defn(self, '$hash', function() {
      var self = this;

      return [self.$$class.$$name,(self.$$class).$__id__(),self.$__id__()].join(':');
    });

    Opal.defn(self, '$initialize_copy', function(other) {
      var self = this;

      return nil;
    });

    Opal.defn(self, '$inspect', function() {
      var self = this;

      return self.$to_s();
    });

    Opal.defn(self, '$instance_of?', function(klass) {
      var self = this;

      return self.$$class === klass;
    });

    Opal.defn(self, '$instance_variable_defined?', function(name) {
      var self = this;

      return Opal.hasOwnProperty.call(self, name.substr(1));
    });

    Opal.defn(self, '$instance_variable_get', function(name) {
      var self = this;

      
      var ivar = self[name.substr(1)];

      return ivar == null ? nil : ivar;
    
    });

    Opal.defn(self, '$instance_variable_set', function(name, value) {
      var self = this;

      return self[name.substr(1)] = value;
    });

    Opal.defn(self, '$instance_variables', function() {
      var self = this;

      
      var result = [];

      for (var name in self) {
        if (name.charAt(0) !== '$') {
          if (name !== '$$class' && name !== '$$id') {
            result.push('@' + name);
          }
        }
      }

      return result;
    
    });

    Opal.defn(self, '$Integer', function(value, base) {
      var $a, $b, self = this, $case = nil;

      if (base == null) {
        base = nil
      }
      if ((($a = $scope.get('String')['$==='](value)) !== nil && (!$a.$$is_boolean || $a == true))) {
        if ((($a = value['$empty?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
          self.$raise($scope.get('ArgumentError'), "invalid value for Integer: (empty string)")};
        return parseInt(value, ((($a = base) !== false && $a !== nil) ? $a : undefined));};
      if (base !== false && base !== nil) {
        self.$raise(self.$ArgumentError("base is only valid for String values"))};
      return (function() {$case = value;if ($scope.get('Integer')['$===']($case)) {return value}else if ($scope.get('Float')['$===']($case)) {if ((($a = ((($b = value['$nan?']()) !== false && $b !== nil) ? $b : value['$infinite?']())) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('FloatDomainError'), "unable to coerce " + (value) + " to Integer")};
      return value.$to_int();}else if ($scope.get('NilClass')['$===']($case)) {return self.$raise($scope.get('TypeError'), "can't convert nil into Integer")}else {if ((($a = value['$respond_to?']("to_int")) !== nil && (!$a.$$is_boolean || $a == true))) {
        return value.$to_int()
      } else if ((($a = value['$respond_to?']("to_i")) !== nil && (!$a.$$is_boolean || $a == true))) {
        return value.$to_i()
        } else {
        return self.$raise($scope.get('TypeError'), "can't convert " + (value.$class()) + " into Integer")
      }}})();
    });

    Opal.defn(self, '$Float', function(value) {
      var $a, self = this;

      if ((($a = $scope.get('String')['$==='](value)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return parseFloat(value);
      } else if ((($a = value['$respond_to?']("to_f")) !== nil && (!$a.$$is_boolean || $a == true))) {
        return value.$to_f()
        } else {
        return self.$raise($scope.get('TypeError'), "can't convert " + (value.$class()) + " into Float")
      };
    });

    Opal.defn(self, '$is_a?', function(klass) {
      var self = this;

      return Opal.is_a(self, klass);
    });

    Opal.defn(self, '$kind_of?', def['$is_a?']);

    Opal.defn(self, '$lambda', TMP_6 = function() {
      var self = this, $iter = TMP_6.$$p, block = $iter || nil;

      TMP_6.$$p = null;
      block.$$is_lambda = true;
      return block;
    });

    Opal.defn(self, '$load', function(file) {
      var self = this;

      file = $scope.get('Opal')['$coerce_to!'](file, $scope.get('String'), "to_str");
      return Opal.load(Opal.normalize_loadable_path(file));
    });

    Opal.defn(self, '$loop', TMP_7 = function() {
      var self = this, $iter = TMP_7.$$p, block = $iter || nil;

      TMP_7.$$p = null;
      
      while (true) {
        if (block() === $breaker) {
          return $breaker.$v;
        }
      }
    
      return self;
    });

    Opal.defn(self, '$nil?', function() {
      var self = this;

      return false;
    });

    Opal.defn(self, '$object_id', def.$__id__);

    Opal.defn(self, '$printf', function(args) {
      var $a, self = this;

      args = $slice.call(arguments, 0);
      if (args.$length()['$>'](0)) {
        self.$print(($a = self).$format.apply($a, [].concat(args)))};
      return nil;
    });

    Opal.defn(self, '$private_methods', function() {
      var self = this;

      return [];
    });

    Opal.defn(self, '$private_instance_methods', def.$private_methods);

    Opal.defn(self, '$proc', TMP_8 = function() {
      var self = this, $iter = TMP_8.$$p, block = $iter || nil;

      TMP_8.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        self.$raise($scope.get('ArgumentError'), "tried to create Proc object without a block")
      };
      block.$$is_lambda = false;
      return block;
    });

    Opal.defn(self, '$puts', function(strs) {
      var $a, self = this;
      if ($gvars.stdout == null) $gvars.stdout = nil;

      strs = $slice.call(arguments, 0);
      return ($a = $gvars.stdout).$puts.apply($a, [].concat(strs));
    });

    Opal.defn(self, '$p', function(args) {
      var $a, $b, TMP_9, self = this;

      args = $slice.call(arguments, 0);
      ($a = ($b = args).$each, $a.$$p = (TMP_9 = function(obj){var self = TMP_9.$$s || this;
        if ($gvars.stdout == null) $gvars.stdout = nil;
if (obj == null) obj = nil;
      return $gvars.stdout.$puts(obj.$inspect())}, TMP_9.$$s = self, TMP_9), $a).call($b);
      if (args.$length()['$<='](1)) {
        return args['$[]'](0)
        } else {
        return args
      };
    });

    Opal.defn(self, '$print', function(strs) {
      var $a, self = this;
      if ($gvars.stdout == null) $gvars.stdout = nil;

      strs = $slice.call(arguments, 0);
      return ($a = $gvars.stdout).$print.apply($a, [].concat(strs));
    });

    Opal.defn(self, '$warn', function(strs) {
      var $a, $b, self = this;
      if ($gvars.VERBOSE == null) $gvars.VERBOSE = nil;
      if ($gvars.stderr == null) $gvars.stderr = nil;

      strs = $slice.call(arguments, 0);
      if ((($a = ((($b = $gvars.VERBOSE['$nil?']()) !== false && $b !== nil) ? $b : strs['$empty?']())) !== nil && (!$a.$$is_boolean || $a == true))) {
        return nil
        } else {
        return ($a = $gvars.stderr).$puts.apply($a, [].concat(strs))
      };
    });

    Opal.defn(self, '$raise', function(exception, string) {
      var self = this;
      if ($gvars["!"] == null) $gvars["!"] = nil;

      
      if (exception == null && $gvars["!"]) {
        exception = $gvars["!"];
      }
      else if (exception.$$is_string) {
        exception = $scope.get('RuntimeError').$new(exception);
      }
      else if (!exception['$is_a?']($scope.get('Exception'))) {
        exception = exception.$new(string);
      }

      $gvars["!"] = exception;
      throw exception;
    ;
    });

    Opal.defn(self, '$fail', def.$raise);

    Opal.defn(self, '$rand', function(max) {
      var self = this;

      
      if (max === undefined) {
        return Math.random();
      }
      else if (max.$$is_range) {
        var arr = max.$to_a();

        return arr[self.$rand(arr.length)];
      }
      else {
        return Math.floor(Math.random() *
          Math.abs($scope.get('Opal').$coerce_to(max, $scope.get('Integer'), "to_int")));
      }
    
    });

    Opal.defn(self, '$respond_to?', function(name, include_all) {
      var $a, self = this;

      if (include_all == null) {
        include_all = false
      }
      if ((($a = self['$respond_to_missing?'](name)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return true};
      
      var body = self['$' + name];

      if (typeof(body) === "function" && !body.$$stub) {
        return true;
      }
    
      return false;
    });

    Opal.defn(self, '$respond_to_missing?', function(method_name) {
      var self = this;

      return false;
    });

    Opal.defn(self, '$require', function(file) {
      var self = this;

      file = $scope.get('Opal')['$coerce_to!'](file, $scope.get('String'), "to_str");
      return Opal.require(Opal.normalize_loadable_path(file));
    });

    Opal.defn(self, '$require_relative', function(file) {
      var self = this;

      $scope.get('Opal')['$try_convert!'](file, $scope.get('String'), "to_str");
      file = $scope.get('File').$expand_path($scope.get('File').$join(Opal.current_file, "..", file));
      return Opal.require(Opal.normalize_loadable_path(file));
    });

    Opal.defn(self, '$require_tree', function(path) {
      var self = this;

      path = $scope.get('File').$expand_path(path);
      
      for (var name in Opal.modules) {
        if ((name)['$start_with?'](path)) {
          Opal.require(name);
        }
      }
    ;
      return nil;
    });

    Opal.defn(self, '$send', def.$__send__);

    Opal.defn(self, '$public_send', def.$__send__);

    Opal.defn(self, '$singleton_class', function() {
      var self = this;

      return Opal.get_singleton_class(self);
    });

    Opal.defn(self, '$sprintf', def.$format);

    Opal.defn(self, '$srand', def.$rand);

    Opal.defn(self, '$String', function(str) {
      var self = this;

      return String(str);
    });

    Opal.defn(self, '$taint', function() {
      var self = this;

      return self;
    });

    Opal.defn(self, '$tainted?', function() {
      var self = this;

      return false;
    });

    Opal.defn(self, '$tap', TMP_10 = function() {
      var self = this, $iter = TMP_10.$$p, block = $iter || nil;

      TMP_10.$$p = null;
      if (Opal.yield1(block, self) === $breaker) return $breaker.$v;
      return self;
    });

    Opal.defn(self, '$to_proc', function() {
      var self = this;

      return self;
    });

    Opal.defn(self, '$to_s', function() {
      var self = this;

      return "#<" + (self.$class()) + ":0x" + (self.$__id__().$to_s(16)) + ">";
    });

    Opal.defn(self, '$untaint', def.$taint);
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["corelib/nil_class"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  Opal.add_stubs(['$raise']);
  (function($base, $super) {
    function $NilClass(){};
    var self = $NilClass = $klass($base, $super, 'NilClass', $NilClass);

    var def = self.$$proto, $scope = self.$$scope;

    def['$!'] = function() {
      var self = this;

      return true;
    };

    def['$&'] = function(other) {
      var self = this;

      return false;
    };

    def['$|'] = function(other) {
      var self = this;

      return other !== false && other !== nil;
    };

    def['$^'] = function(other) {
      var self = this;

      return other !== false && other !== nil;
    };

    def['$=='] = function(other) {
      var self = this;

      return other === nil;
    };

    def.$dup = function() {
      var self = this;

      return self.$raise($scope.get('TypeError'));
    };

    def.$inspect = function() {
      var self = this;

      return "nil";
    };

    def['$nil?'] = function() {
      var self = this;

      return true;
    };

    def.$singleton_class = function() {
      var self = this;

      return $scope.get('NilClass');
    };

    def.$to_a = function() {
      var self = this;

      return [];
    };

    def.$to_h = function() {
      var self = this;

      return Opal.hash();
    };

    def.$to_i = function() {
      var self = this;

      return 0;
    };

    Opal.defn(self, '$to_f', def.$to_i);

    return (def.$to_s = function() {
      var self = this;

      return "";
    }, nil) && 'to_s';
  })(self, null);
  return Opal.cdecl($scope, 'NIL', nil);
};

/* Generated by Opal 0.7.2 */
Opal.modules["corelib/boolean"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  Opal.add_stubs(['$undef_method']);
  (function($base, $super) {
    function $Boolean(){};
    var self = $Boolean = $klass($base, $super, 'Boolean', $Boolean);

    var def = self.$$proto, $scope = self.$$scope;

    def.$$is_boolean = true;

    (function(self) {
      var $scope = self.$$scope, def = self.$$proto;

      return self.$undef_method("new")
    })(self.$singleton_class());

    def['$!'] = function() {
      var self = this;

      return self != true;
    };

    def['$&'] = function(other) {
      var self = this;

      return (self == true) ? (other !== false && other !== nil) : false;
    };

    def['$|'] = function(other) {
      var self = this;

      return (self == true) ? true : (other !== false && other !== nil);
    };

    def['$^'] = function(other) {
      var self = this;

      return (self == true) ? (other === false || other === nil) : (other !== false && other !== nil);
    };

    def['$=='] = function(other) {
      var self = this;

      return (self == true) === other.valueOf();
    };

    Opal.defn(self, '$equal?', def['$==']);

    Opal.defn(self, '$singleton_class', def.$class);

    return (def.$to_s = function() {
      var self = this;

      return (self == true) ? 'true' : 'false';
    }, nil) && 'to_s';
  })(self, null);
  Opal.cdecl($scope, 'TrueClass', $scope.get('Boolean'));
  Opal.cdecl($scope, 'FalseClass', $scope.get('Boolean'));
  Opal.cdecl($scope, 'TRUE', true);
  return Opal.cdecl($scope, 'FALSE', false);
};

/* Generated by Opal 0.7.2 */
Opal.modules["corelib/error"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass, $module = Opal.module;

  Opal.add_stubs(['$attr_reader', '$class']);
  (function($base, $super) {
    function $Exception(){};
    var self = $Exception = $klass($base, $super, 'Exception', $Exception);

    var def = self.$$proto, $scope = self.$$scope;

    def.message = nil;
    self.$attr_reader("message");

    Opal.defs(self, '$new', function(message) {
      var self = this;

      if (message == null) {
        message = "Exception"
      }
      
      var err = new self.$$alloc(message);

      if (Error.captureStackTrace) {
        Error.captureStackTrace(err);
      }

      err.name = self.$$name;
      err.$initialize(message);
      return err;
    
    });

    def.$initialize = function(message) {
      var self = this;

      return self.message = message;
    };

    def.$backtrace = function() {
      var self = this;

      
      var backtrace = self.stack;

      if (typeof(backtrace) === 'string') {
        return backtrace.split("\n").slice(0, 15);
      }
      else if (backtrace) {
        return backtrace.slice(0, 15);
      }

      return [];
    
    };

    def.$inspect = function() {
      var self = this;

      return "#<" + (self.$class()) + ": '" + (self.message) + "'>";
    };

    return Opal.defn(self, '$to_s', def.$message);
  })(self, null);
  (function($base, $super) {
    function $ScriptError(){};
    var self = $ScriptError = $klass($base, $super, 'ScriptError', $ScriptError);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('Exception'));
  (function($base, $super) {
    function $SyntaxError(){};
    var self = $SyntaxError = $klass($base, $super, 'SyntaxError', $SyntaxError);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('ScriptError'));
  (function($base, $super) {
    function $LoadError(){};
    var self = $LoadError = $klass($base, $super, 'LoadError', $LoadError);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('ScriptError'));
  (function($base, $super) {
    function $NotImplementedError(){};
    var self = $NotImplementedError = $klass($base, $super, 'NotImplementedError', $NotImplementedError);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('ScriptError'));
  (function($base, $super) {
    function $SystemExit(){};
    var self = $SystemExit = $klass($base, $super, 'SystemExit', $SystemExit);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('Exception'));
  (function($base, $super) {
    function $NoMemoryError(){};
    var self = $NoMemoryError = $klass($base, $super, 'NoMemoryError', $NoMemoryError);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('Exception'));
  (function($base, $super) {
    function $SignalException(){};
    var self = $SignalException = $klass($base, $super, 'SignalException', $SignalException);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('Exception'));
  (function($base, $super) {
    function $Interrupt(){};
    var self = $Interrupt = $klass($base, $super, 'Interrupt', $Interrupt);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('Exception'));
  (function($base, $super) {
    function $StandardError(){};
    var self = $StandardError = $klass($base, $super, 'StandardError', $StandardError);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('Exception'));
  (function($base, $super) {
    function $NameError(){};
    var self = $NameError = $klass($base, $super, 'NameError', $NameError);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('StandardError'));
  (function($base, $super) {
    function $NoMethodError(){};
    var self = $NoMethodError = $klass($base, $super, 'NoMethodError', $NoMethodError);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('NameError'));
  (function($base, $super) {
    function $RuntimeError(){};
    var self = $RuntimeError = $klass($base, $super, 'RuntimeError', $RuntimeError);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('StandardError'));
  (function($base, $super) {
    function $LocalJumpError(){};
    var self = $LocalJumpError = $klass($base, $super, 'LocalJumpError', $LocalJumpError);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('StandardError'));
  (function($base, $super) {
    function $TypeError(){};
    var self = $TypeError = $klass($base, $super, 'TypeError', $TypeError);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('StandardError'));
  (function($base, $super) {
    function $ArgumentError(){};
    var self = $ArgumentError = $klass($base, $super, 'ArgumentError', $ArgumentError);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('StandardError'));
  (function($base, $super) {
    function $IndexError(){};
    var self = $IndexError = $klass($base, $super, 'IndexError', $IndexError);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('StandardError'));
  (function($base, $super) {
    function $StopIteration(){};
    var self = $StopIteration = $klass($base, $super, 'StopIteration', $StopIteration);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('IndexError'));
  (function($base, $super) {
    function $KeyError(){};
    var self = $KeyError = $klass($base, $super, 'KeyError', $KeyError);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('IndexError'));
  (function($base, $super) {
    function $RangeError(){};
    var self = $RangeError = $klass($base, $super, 'RangeError', $RangeError);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('StandardError'));
  (function($base, $super) {
    function $FloatDomainError(){};
    var self = $FloatDomainError = $klass($base, $super, 'FloatDomainError', $FloatDomainError);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('RangeError'));
  (function($base, $super) {
    function $IOError(){};
    var self = $IOError = $klass($base, $super, 'IOError', $IOError);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('StandardError'));
  (function($base, $super) {
    function $SystemCallError(){};
    var self = $SystemCallError = $klass($base, $super, 'SystemCallError', $SystemCallError);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('StandardError'));
  return (function($base) {
    var self = $module($base, 'Errno');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base, $super) {
      function $EINVAL(){};
      var self = $EINVAL = $klass($base, $super, 'EINVAL', $EINVAL);

      var def = self.$$proto, $scope = self.$$scope, TMP_1;

      return (Opal.defs(self, '$new', TMP_1 = function() {
        var self = this, $iter = TMP_1.$$p, $yield = $iter || nil;

        TMP_1.$$p = null;
        return Opal.find_super_dispatcher(self, 'new', TMP_1, null, $EINVAL).apply(self, ["Invalid argument"]);
      }), nil) && 'new'
    })(self, $scope.get('SystemCallError'))
  })(self);
};

/* Generated by Opal 0.7.2 */
Opal.modules["corelib/regexp"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass, $gvars = Opal.gvars;

  Opal.add_stubs(['$nil?', '$[]', '$respond_to?', '$to_str', '$to_s', '$coerce_to', '$new', '$raise', '$class', '$call']);
  return (function($base, $super) {
    function $Regexp(){};
    var self = $Regexp = $klass($base, $super, 'Regexp', $Regexp);

    var def = self.$$proto, $scope = self.$$scope, TMP_1;

    def.$$is_regexp = true;

    (function(self) {
      var $scope = self.$$scope, def = self.$$proto;

      self.$$proto.$escape = function(string) {
        var self = this;

        
        return string.replace(/([-[\]\/{}()*+?.^$\\| ])/g, '\\$1')
                     .replace(/[\n]/g, '\\n')
                     .replace(/[\r]/g, '\\r')
                     .replace(/[\f]/g, '\\f')
                     .replace(/[\t]/g, '\\t');
      
      };
      self.$$proto.$last_match = function(n) {
        var $a, self = this;
        if ($gvars["~"] == null) $gvars["~"] = nil;

        if (n == null) {
          n = nil
        }
        if ((($a = n['$nil?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
          return $gvars["~"]
          } else {
          return $gvars["~"]['$[]'](n)
        };
      };
      self.$$proto.$quote = self.$$proto.$escape;
      self.$$proto.$union = function(parts) {
        var self = this;

        parts = $slice.call(arguments, 0);
        return new RegExp(parts.join(''));
      };
      return (self.$$proto.$new = function(regexp, options) {
        var self = this;

        return new RegExp(regexp, options);
      }, nil) && 'new';
    })(self.$singleton_class());

    def['$=='] = function(other) {
      var self = this;

      return other.constructor == RegExp && self.toString() === other.toString();
    };

    def['$==='] = function(str) {
      var self = this;

      
      if (!str.$$is_string && str['$respond_to?']("to_str")) {
        str = str.$to_str();
      }

      if (!str.$$is_string) {
        return false;
      }

      return self.test(str);
    ;
    };

    def['$=~'] = function(string) {
      var $a, self = this;

      if ((($a = string === nil) !== nil && (!$a.$$is_boolean || $a == true))) {
        $gvars["~"] = nil;
        return nil;};
      string = $scope.get('Opal').$coerce_to(string, $scope.get('String'), "to_str").$to_s();
      
      var re = self;

      if (re.global) {
        // should we clear it afterwards too?
        re.lastIndex = 0;
      }
      else {
        // rewrite regular expression to add the global flag to capture pre/post match
        re = new RegExp(re.source, 'g' + (re.multiline ? 'm' : '') + (re.ignoreCase ? 'i' : ''));
      }

      var result = re.exec(string);

      if (result) {
        $gvars["~"] = $scope.get('MatchData').$new(re, result);

        return result.index;
      }
      else {
        $gvars["~"] = nil;
        return nil;
      }
    
    };

    Opal.defn(self, '$eql?', def['$==']);

    def.$inspect = function() {
      var self = this;

      return self.toString();
    };

    def.$match = TMP_1 = function(string, pos) {
      var $a, self = this, $iter = TMP_1.$$p, block = $iter || nil;

      TMP_1.$$p = null;
      if ((($a = string === nil) !== nil && (!$a.$$is_boolean || $a == true))) {
        $gvars["~"] = nil;
        return nil;};
      if ((($a = string.$$is_string == null) !== nil && (!$a.$$is_boolean || $a == true))) {
        if ((($a = string['$respond_to?']("to_str")) !== nil && (!$a.$$is_boolean || $a == true))) {
          } else {
          self.$raise($scope.get('TypeError'), "no implicit conversion of " + (string.$class()) + " into String")
        };
        string = string.$to_str();};
      
      var re = self;

      if (re.global) {
        // should we clear it afterwards too?
        re.lastIndex = 0;
      }
      else {
        re = new RegExp(re.source, 'g' + (re.multiline ? 'm' : '') + (re.ignoreCase ? 'i' : ''));
      }

      var result = re.exec(string);

      if (result) {
        result = $gvars["~"] = $scope.get('MatchData').$new(re, result);

        if (block === nil) {
          return result;
        }
        else {
          return block.$call(result);
        }
      }
      else {
        return $gvars["~"] = nil;
      }
    
    };

    def.$source = function() {
      var self = this;

      return self.source;
    };

    return Opal.defn(self, '$to_s', def.$source);
  })(self, null)
};

/* Generated by Opal 0.7.2 */
Opal.modules["corelib/comparable"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module;

  Opal.add_stubs(['$===', '$>', '$<', '$equal?', '$<=>', '$normalize', '$raise', '$class']);
  return (function($base) {
    var self = $module($base, 'Comparable');

    var def = self.$$proto, $scope = self.$$scope;

    Opal.defs(self, '$normalize', function(what) {
      var $a, self = this;

      if ((($a = $scope.get('Integer')['$==='](what)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return what};
      if (what['$>'](0)) {
        return 1};
      if (what['$<'](0)) {
        return -1};
      return 0;
    });

    Opal.defn(self, '$==', function(other) {
      var $a, self = this, cmp = nil;

      try {
      if ((($a = self['$equal?'](other)) !== nil && (!$a.$$is_boolean || $a == true))) {
          return true};
        if ((($a = cmp = (self['$<=>'](other))) !== nil && (!$a.$$is_boolean || $a == true))) {
          } else {
          return false
        };
        return $scope.get('Comparable').$normalize(cmp) == 0;
      } catch ($err) {if (Opal.rescue($err, [$scope.get('StandardError')])) {
        return false
        }else { throw $err; }
      };
    });

    Opal.defn(self, '$>', function(other) {
      var $a, self = this, cmp = nil;

      if ((($a = cmp = (self['$<=>'](other))) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise($scope.get('ArgumentError'), "comparison of " + (self.$class()) + " with " + (other.$class()) + " failed")
      };
      return $scope.get('Comparable').$normalize(cmp) > 0;
    });

    Opal.defn(self, '$>=', function(other) {
      var $a, self = this, cmp = nil;

      if ((($a = cmp = (self['$<=>'](other))) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise($scope.get('ArgumentError'), "comparison of " + (self.$class()) + " with " + (other.$class()) + " failed")
      };
      return $scope.get('Comparable').$normalize(cmp) >= 0;
    });

    Opal.defn(self, '$<', function(other) {
      var $a, self = this, cmp = nil;

      if ((($a = cmp = (self['$<=>'](other))) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise($scope.get('ArgumentError'), "comparison of " + (self.$class()) + " with " + (other.$class()) + " failed")
      };
      return $scope.get('Comparable').$normalize(cmp) < 0;
    });

    Opal.defn(self, '$<=', function(other) {
      var $a, self = this, cmp = nil;

      if ((($a = cmp = (self['$<=>'](other))) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise($scope.get('ArgumentError'), "comparison of " + (self.$class()) + " with " + (other.$class()) + " failed")
      };
      return $scope.get('Comparable').$normalize(cmp) <= 0;
    });

    Opal.defn(self, '$between?', function(min, max) {
      var self = this;

      if (self['$<'](min)) {
        return false};
      if (self['$>'](max)) {
        return false};
      return true;
    });
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["corelib/enumerable"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module;

  Opal.add_stubs(['$raise', '$enum_for', '$flatten', '$map', '$==', '$destructure', '$nil?', '$coerce_to!', '$coerce_to', '$===', '$new', '$<<', '$[]', '$[]=', '$inspect', '$__send__', '$yield', '$enumerator_size', '$respond_to?', '$size', '$private', '$compare', '$<=>', '$dup', '$to_a', '$lambda', '$sort', '$call', '$first', '$zip']);
  return (function($base) {
    var self = $module($base, 'Enumerable');

    var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_2, TMP_3, TMP_4, TMP_5, TMP_7, TMP_8, TMP_9, TMP_10, TMP_11, TMP_12, TMP_13, TMP_14, TMP_15, TMP_16, TMP_17, TMP_18, TMP_19, TMP_20, TMP_22, TMP_23, TMP_24, TMP_25, TMP_26, TMP_27, TMP_28, TMP_29, TMP_30, TMP_31, TMP_32, TMP_33, TMP_35, TMP_37, TMP_41, TMP_42;

    Opal.defn(self, '$all?', TMP_1 = function() {
      var $a, self = this, $iter = TMP_1.$$p, block = $iter || nil;

      TMP_1.$$p = null;
      
      var result = true;

      if (block !== nil) {
        self.$each.$$p = function() {
          var value = Opal.yieldX(block, arguments);

          if (value === $breaker) {
            result = $breaker.$v;
            return $breaker;
          }

          if ((($a = value) === nil || ($a.$$is_boolean && $a == false))) {
            result = false;
            return $breaker;
          }
        };
      }
      else {
        self.$each.$$p = function(obj) {
          if (arguments.length == 1 && (($a = obj) === nil || ($a.$$is_boolean && $a == false))) {
            result = false;
            return $breaker;
          }
        };
      }

      self.$each();

      return result;
    
    });

    Opal.defn(self, '$any?', TMP_2 = function() {
      var $a, self = this, $iter = TMP_2.$$p, block = $iter || nil;

      TMP_2.$$p = null;
      
      var result = false;

      if (block !== nil) {
        self.$each.$$p = function() {
          var value = Opal.yieldX(block, arguments);

          if (value === $breaker) {
            result = $breaker.$v;
            return $breaker;
          }

          if ((($a = value) !== nil && (!$a.$$is_boolean || $a == true))) {
            result = true;
            return $breaker;
          }
        };
      }
      else {
        self.$each.$$p = function(obj) {
          if (arguments.length != 1 || (($a = obj) !== nil && (!$a.$$is_boolean || $a == true))) {
            result = true;
            return $breaker;
          }
        }
      }

      self.$each();

      return result;
    
    });

    Opal.defn(self, '$chunk', TMP_3 = function(state) {
      var self = this, $iter = TMP_3.$$p, block = $iter || nil;

      TMP_3.$$p = null;
      return self.$raise($scope.get('NotImplementedError'));
    });

    Opal.defn(self, '$collect', TMP_4 = function() {
      var self = this, $iter = TMP_4.$$p, block = $iter || nil;

      TMP_4.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("collect")
      };
      
      var result = [];

      self.$each.$$p = function() {
        var value = Opal.yieldX(block, arguments);

        if (value === $breaker) {
          result = $breaker.$v;
          return $breaker;
        }

        result.push(value);
      };

      self.$each();

      return result;
    
    });

    Opal.defn(self, '$collect_concat', TMP_5 = function() {
      var $a, $b, TMP_6, self = this, $iter = TMP_5.$$p, block = $iter || nil;

      TMP_5.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("collect_concat")
      };
      return ($a = ($b = self).$map, $a.$$p = (TMP_6 = function(item){var self = TMP_6.$$s || this, $a;
if (item == null) item = nil;
      return $a = Opal.yield1(block, item), $a === $breaker ? $a : $a}, TMP_6.$$s = self, TMP_6), $a).call($b).$flatten(1);
    });

    Opal.defn(self, '$count', TMP_7 = function(object) {
      var $a, self = this, $iter = TMP_7.$$p, block = $iter || nil;

      TMP_7.$$p = null;
      
      var result = 0;

      if (object != null) {
        block = function() {
          return $scope.get('Opal').$destructure(arguments)['$=='](object);
        };
      }
      else if (block === nil) {
        block = function() { return true; };
      }

      self.$each.$$p = function() {
        var value = Opal.yieldX(block, arguments);

        if (value === $breaker) {
          result = $breaker.$v;
          return $breaker;
        }

        if ((($a = value) !== nil && (!$a.$$is_boolean || $a == true))) {
          result++;
        }
      }

      self.$each();

      return result;
    
    });

    Opal.defn(self, '$cycle', TMP_8 = function(n) {
      var $a, self = this, $iter = TMP_8.$$p, block = $iter || nil;

      if (n == null) {
        n = nil
      }
      TMP_8.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        return self.$enum_for("cycle", n)
      };
      if ((($a = n['$nil?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        n = $scope.get('Opal')['$coerce_to!'](n, $scope.get('Integer'), "to_int");
        if ((($a = n <= 0) !== nil && (!$a.$$is_boolean || $a == true))) {
          return nil};
      };
      
      var result,
          all  = [];

      self.$each.$$p = function() {
        var param = $scope.get('Opal').$destructure(arguments),
            value = Opal.yield1(block, param);

        if (value === $breaker) {
          result = $breaker.$v;
          return $breaker;
        }

        all.push(param);
      }

      self.$each();

      if (result !== undefined) {
        return result;
      }

      if (all.length === 0) {
        return nil;
      }
    
      if ((($a = n['$nil?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
        
        while (true) {
          for (var i = 0, length = all.length; i < length; i++) {
            var value = Opal.yield1(block, all[i]);

            if (value === $breaker) {
              return $breaker.$v;
            }
          }
        }
      
        } else {
        
        while (n > 1) {
          for (var i = 0, length = all.length; i < length; i++) {
            var value = Opal.yield1(block, all[i]);

            if (value === $breaker) {
              return $breaker.$v;
            }
          }

          n--;
        }
      
      };
    });

    Opal.defn(self, '$detect', TMP_9 = function(ifnone) {
      var $a, self = this, $iter = TMP_9.$$p, block = $iter || nil;

      TMP_9.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("detect", ifnone)
      };
      
      var result = undefined;

      self.$each.$$p = function() {
        var params = $scope.get('Opal').$destructure(arguments),
            value  = Opal.yield1(block, params);

        if (value === $breaker) {
          result = $breaker.$v;
          return $breaker;
        }

        if ((($a = value) !== nil && (!$a.$$is_boolean || $a == true))) {
          result = params;
          return $breaker;
        }
      };

      self.$each();

      if (result === undefined && ifnone !== undefined) {
        if (typeof(ifnone) === 'function') {
          result = ifnone();
        }
        else {
          result = ifnone;
        }
      }

      return result === undefined ? nil : result;
    
    });

    Opal.defn(self, '$drop', function(number) {
      var $a, self = this;

      number = $scope.get('Opal').$coerce_to(number, $scope.get('Integer'), "to_int");
      if ((($a = number < 0) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('ArgumentError'), "attempt to drop negative size")};
      
      var result  = [],
          current = 0;

      self.$each.$$p = function() {
        if (number <= current) {
          result.push($scope.get('Opal').$destructure(arguments));
        }

        current++;
      };

      self.$each()

      return result;
    
    });

    Opal.defn(self, '$drop_while', TMP_10 = function() {
      var $a, self = this, $iter = TMP_10.$$p, block = $iter || nil;

      TMP_10.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("drop_while")
      };
      
      var result   = [],
          dropping = true;

      self.$each.$$p = function() {
        var param = $scope.get('Opal').$destructure(arguments);

        if (dropping) {
          var value = Opal.yield1(block, param);

          if (value === $breaker) {
            result = $breaker.$v;
            return $breaker;
          }

          if ((($a = value) === nil || ($a.$$is_boolean && $a == false))) {
            dropping = false;
            result.push(param);
          }
        }
        else {
          result.push(param);
        }
      };

      self.$each();

      return result;
    
    });

    Opal.defn(self, '$each_cons', TMP_11 = function(n) {
      var self = this, $iter = TMP_11.$$p, block = $iter || nil;

      TMP_11.$$p = null;
      return self.$raise($scope.get('NotImplementedError'));
    });

    Opal.defn(self, '$each_entry', TMP_12 = function() {
      var self = this, $iter = TMP_12.$$p, block = $iter || nil;

      TMP_12.$$p = null;
      return self.$raise($scope.get('NotImplementedError'));
    });

    Opal.defn(self, '$each_slice', TMP_13 = function(n) {
      var $a, self = this, $iter = TMP_13.$$p, block = $iter || nil;

      TMP_13.$$p = null;
      n = $scope.get('Opal').$coerce_to(n, $scope.get('Integer'), "to_int");
      if ((($a = n <= 0) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('ArgumentError'), "invalid slice size")};
      if ((block !== nil)) {
        } else {
        return self.$enum_for("each_slice", n)
      };
      
      var result,
          slice = []

      self.$each.$$p = function() {
        var param = $scope.get('Opal').$destructure(arguments);

        slice.push(param);

        if (slice.length === n) {
          if (Opal.yield1(block, slice) === $breaker) {
            result = $breaker.$v;
            return $breaker;
          }

          slice = [];
        }
      };

      self.$each();

      if (result !== undefined) {
        return result;
      }

      // our "last" group, if smaller than n then won't have been yielded
      if (slice.length > 0) {
        if (Opal.yield1(block, slice) === $breaker) {
          return $breaker.$v;
        }
      }
    ;
      return nil;
    });

    Opal.defn(self, '$each_with_index', TMP_14 = function(args) {
      var $a, self = this, $iter = TMP_14.$$p, block = $iter || nil;

      args = $slice.call(arguments, 0);
      TMP_14.$$p = null;
      if ((block !== nil)) {
        } else {
        return ($a = self).$enum_for.apply($a, ["each_with_index"].concat(args))
      };
      
      var result,
          index = 0;

      self.$each.$$p = function() {
        var param = $scope.get('Opal').$destructure(arguments),
            value = block(param, index);

        if (value === $breaker) {
          result = $breaker.$v;
          return $breaker;
        }

        index++;
      };

      self.$each.apply(self, args);

      if (result !== undefined) {
        return result;
      }
    
      return self;
    });

    Opal.defn(self, '$each_with_object', TMP_15 = function(object) {
      var self = this, $iter = TMP_15.$$p, block = $iter || nil;

      TMP_15.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("each_with_object", object)
      };
      
      var result;

      self.$each.$$p = function() {
        var param = $scope.get('Opal').$destructure(arguments),
            value = block(param, object);

        if (value === $breaker) {
          result = $breaker.$v;
          return $breaker;
        }
      };

      self.$each();

      if (result !== undefined) {
        return result;
      }
    
      return object;
    });

    Opal.defn(self, '$entries', function(args) {
      var self = this;

      args = $slice.call(arguments, 0);
      
      var result = [];

      self.$each.$$p = function() {
        result.push($scope.get('Opal').$destructure(arguments));
      };

      self.$each.apply(self, args);

      return result;
    
    });

    Opal.defn(self, '$find', def.$detect);

    Opal.defn(self, '$find_all', TMP_16 = function() {
      var $a, self = this, $iter = TMP_16.$$p, block = $iter || nil;

      TMP_16.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("find_all")
      };
      
      var result = [];

      self.$each.$$p = function() {
        var param = $scope.get('Opal').$destructure(arguments),
            value = Opal.yield1(block, param);

        if (value === $breaker) {
          result = $breaker.$v;
          return $breaker;
        }

        if ((($a = value) !== nil && (!$a.$$is_boolean || $a == true))) {
          result.push(param);
        }
      };

      self.$each();

      return result;
    
    });

    Opal.defn(self, '$find_index', TMP_17 = function(object) {
      var $a, self = this, $iter = TMP_17.$$p, block = $iter || nil;

      TMP_17.$$p = null;
      if ((($a = object === undefined && block === nil) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self.$enum_for("find_index")};
      
      var result = nil,
          index  = 0;

      if (object != null) {
        self.$each.$$p = function() {
          var param = $scope.get('Opal').$destructure(arguments);

          if ((param)['$=='](object)) {
            result = index;
            return $breaker;
          }

          index += 1;
        };
      }
      else if (block !== nil) {
        self.$each.$$p = function() {
          var value = Opal.yieldX(block, arguments);

          if (value === $breaker) {
            result = $breaker.$v;
            return $breaker;
          }

          if ((($a = value) !== nil && (!$a.$$is_boolean || $a == true))) {
            result = index;
            return $breaker;
          }

          index += 1;
        };
      }

      self.$each();

      return result;
    
    });

    Opal.defn(self, '$first', function(number) {
      var $a, self = this, result = nil;

      if ((($a = number === undefined) !== nil && (!$a.$$is_boolean || $a == true))) {
        result = nil;
        
        self.$each.$$p = function() {
          result = $scope.get('Opal').$destructure(arguments);

          return $breaker;
        };

        self.$each();
      ;
        } else {
        result = [];
        number = $scope.get('Opal').$coerce_to(number, $scope.get('Integer'), "to_int");
        if ((($a = number < 0) !== nil && (!$a.$$is_boolean || $a == true))) {
          self.$raise($scope.get('ArgumentError'), "attempt to take negative size")};
        if ((($a = number == 0) !== nil && (!$a.$$is_boolean || $a == true))) {
          return []};
        
        var current = 0,
            number  = $scope.get('Opal').$coerce_to(number, $scope.get('Integer'), "to_int");

        self.$each.$$p = function() {
          result.push($scope.get('Opal').$destructure(arguments));

          if (number <= ++current) {
            return $breaker;
          }
        };

        self.$each();
      ;
      };
      return result;
    });

    Opal.defn(self, '$flat_map', def.$collect_concat);

    Opal.defn(self, '$grep', TMP_18 = function(pattern) {
      var $a, self = this, $iter = TMP_18.$$p, block = $iter || nil;

      TMP_18.$$p = null;
      
      var result = [];

      if (block !== nil) {
        self.$each.$$p = function() {
          var param = $scope.get('Opal').$destructure(arguments),
              value = pattern['$==='](param);

          if ((($a = value) !== nil && (!$a.$$is_boolean || $a == true))) {
            value = Opal.yield1(block, param);

            if (value === $breaker) {
              result = $breaker.$v;
              return $breaker;
            }

            result.push(value);
          }
        };
      }
      else {
        self.$each.$$p = function() {
          var param = $scope.get('Opal').$destructure(arguments),
              value = pattern['$==='](param);

          if ((($a = value) !== nil && (!$a.$$is_boolean || $a == true))) {
            result.push(param);
          }
        };
      }

      self.$each();

      return result;
    ;
    });

    Opal.defn(self, '$group_by', TMP_19 = function() {
      var $a, $b, $c, self = this, $iter = TMP_19.$$p, block = $iter || nil, hash = nil;

      TMP_19.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("group_by")
      };
      hash = $scope.get('Hash').$new();
      
      var result;

      self.$each.$$p = function() {
        var param = $scope.get('Opal').$destructure(arguments),
            value = Opal.yield1(block, param);

        if (value === $breaker) {
          result = $breaker.$v;
          return $breaker;
        }

        (($a = value, $b = hash, ((($c = $b['$[]']($a)) !== false && $c !== nil) ? $c : $b['$[]=']($a, []))))['$<<'](param);
      }

      self.$each();

      if (result !== undefined) {
        return result;
      }
    
      return hash;
    });

    Opal.defn(self, '$include?', function(obj) {
      var self = this;

      
      var result = false;

      self.$each.$$p = function() {
        var param = $scope.get('Opal').$destructure(arguments);

        if ((param)['$=='](obj)) {
          result = true;
          return $breaker;
        }
      }

      self.$each();

      return result;
    
    });

    Opal.defn(self, '$inject', TMP_20 = function(object, sym) {
      var self = this, $iter = TMP_20.$$p, block = $iter || nil;

      TMP_20.$$p = null;
      
      var result = object;

      if (block !== nil && sym === undefined) {
        self.$each.$$p = function() {
          var value = $scope.get('Opal').$destructure(arguments);

          if (result === undefined) {
            result = value;
            return;
          }

          value = Opal.yieldX(block, [result, value]);

          if (value === $breaker) {
            result = $breaker.$v;
            return $breaker;
          }

          result = value;
        };
      }
      else {
        if (sym === undefined) {
          if (!$scope.get('Symbol')['$==='](object)) {
            self.$raise($scope.get('TypeError'), "" + (object.$inspect()) + " is not a Symbol");
          }

          sym    = object;
          result = undefined;
        }

        self.$each.$$p = function() {
          var value = $scope.get('Opal').$destructure(arguments);

          if (result === undefined) {
            result = value;
            return;
          }

          result = (result).$__send__(sym, value);
        };
      }

      self.$each();

      return result == undefined ? nil : result;
    ;
    });

    Opal.defn(self, '$lazy', function() {
      var $a, $b, TMP_21, self = this;

      return ($a = ($b = (($scope.get('Enumerator')).$$scope.get('Lazy'))).$new, $a.$$p = (TMP_21 = function(enum$, args){var self = TMP_21.$$s || this, $a;
if (enum$ == null) enum$ = nil;args = $slice.call(arguments, 1);
      return ($a = enum$).$yield.apply($a, [].concat(args))}, TMP_21.$$s = self, TMP_21), $a).call($b, self, self.$enumerator_size());
    });

    Opal.defn(self, '$enumerator_size', function() {
      var $a, self = this;

      if ((($a = self['$respond_to?']("size")) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self.$size()
        } else {
        return nil
      };
    });

    self.$private("enumerator_size");

    Opal.defn(self, '$map', def.$collect);

    Opal.defn(self, '$max', TMP_22 = function() {
      var self = this, $iter = TMP_22.$$p, block = $iter || nil;

      TMP_22.$$p = null;
      
      var result;

      if (block !== nil) {
        self.$each.$$p = function() {
          var param = $scope.get('Opal').$destructure(arguments);

          if (result === undefined) {
            result = param;
            return;
          }

          var value = block(param, result);

          if (value === $breaker) {
            result = $breaker.$v;
            return $breaker;
          }

          if (value === nil) {
            self.$raise($scope.get('ArgumentError'), "comparison failed");
          }

          if (value > 0) {
            result = param;
          }
        };
      }
      else {
        self.$each.$$p = function() {
          var param = $scope.get('Opal').$destructure(arguments);

          if (result === undefined) {
            result = param;
            return;
          }

          if ($scope.get('Opal').$compare(param, result) > 0) {
            result = param;
          }
        };
      }

      self.$each();

      return result === undefined ? nil : result;
    
    });

    Opal.defn(self, '$max_by', TMP_23 = function() {
      var self = this, $iter = TMP_23.$$p, block = $iter || nil;

      TMP_23.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        return self.$enum_for("max_by")
      };
      
      var result,
          by;

      self.$each.$$p = function() {
        var param = $scope.get('Opal').$destructure(arguments),
            value = Opal.yield1(block, param);

        if (result === undefined) {
          result = param;
          by     = value;
          return;
        }

        if (value === $breaker) {
          result = $breaker.$v;
          return $breaker;
        }

        if ((value)['$<=>'](by) > 0) {
          result = param
          by     = value;
        }
      };

      self.$each();

      return result === undefined ? nil : result;
    
    });

    Opal.defn(self, '$member?', def['$include?']);

    Opal.defn(self, '$min', TMP_24 = function() {
      var self = this, $iter = TMP_24.$$p, block = $iter || nil;

      TMP_24.$$p = null;
      
      var result;

      if (block !== nil) {
        self.$each.$$p = function() {
          var param = $scope.get('Opal').$destructure(arguments);

          if (result === undefined) {
            result = param;
            return;
          }

          var value = block(param, result);

          if (value === $breaker) {
            result = $breaker.$v;
            return $breaker;
          }

          if (value === nil) {
            self.$raise($scope.get('ArgumentError'), "comparison failed");
          }

          if (value < 0) {
            result = param;
          }
        };
      }
      else {
        self.$each.$$p = function() {
          var param = $scope.get('Opal').$destructure(arguments);

          if (result === undefined) {
            result = param;
            return;
          }

          if ($scope.get('Opal').$compare(param, result) < 0) {
            result = param;
          }
        };
      }

      self.$each();

      return result === undefined ? nil : result;
    
    });

    Opal.defn(self, '$min_by', TMP_25 = function() {
      var self = this, $iter = TMP_25.$$p, block = $iter || nil;

      TMP_25.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        return self.$enum_for("min_by")
      };
      
      var result,
          by;

      self.$each.$$p = function() {
        var param = $scope.get('Opal').$destructure(arguments),
            value = Opal.yield1(block, param);

        if (result === undefined) {
          result = param;
          by     = value;
          return;
        }

        if (value === $breaker) {
          result = $breaker.$v;
          return $breaker;
        }

        if ((value)['$<=>'](by) < 0) {
          result = param
          by     = value;
        }
      };

      self.$each();

      return result === undefined ? nil : result;
    
    });

    Opal.defn(self, '$minmax', TMP_26 = function() {
      var self = this, $iter = TMP_26.$$p, block = $iter || nil;

      TMP_26.$$p = null;
      return self.$raise($scope.get('NotImplementedError'));
    });

    Opal.defn(self, '$minmax_by', TMP_27 = function() {
      var self = this, $iter = TMP_27.$$p, block = $iter || nil;

      TMP_27.$$p = null;
      return self.$raise($scope.get('NotImplementedError'));
    });

    Opal.defn(self, '$none?', TMP_28 = function() {
      var $a, self = this, $iter = TMP_28.$$p, block = $iter || nil;

      TMP_28.$$p = null;
      
      var result = true;

      if (block !== nil) {
        self.$each.$$p = function() {
          var value = Opal.yieldX(block, arguments);

          if (value === $breaker) {
            result = $breaker.$v;
            return $breaker;
          }

          if ((($a = value) !== nil && (!$a.$$is_boolean || $a == true))) {
            result = false;
            return $breaker;
          }
        }
      }
      else {
        self.$each.$$p = function() {
          var value = $scope.get('Opal').$destructure(arguments);

          if ((($a = value) !== nil && (!$a.$$is_boolean || $a == true))) {
            result = false;
            return $breaker;
          }
        };
      }

      self.$each();

      return result;
    
    });

    Opal.defn(self, '$one?', TMP_29 = function() {
      var $a, self = this, $iter = TMP_29.$$p, block = $iter || nil;

      TMP_29.$$p = null;
      
      var result = false;

      if (block !== nil) {
        self.$each.$$p = function() {
          var value = Opal.yieldX(block, arguments);

          if (value === $breaker) {
            result = $breaker.$v;
            return $breaker;
          }

          if ((($a = value) !== nil && (!$a.$$is_boolean || $a == true))) {
            if (result === true) {
              result = false;
              return $breaker;
            }

            result = true;
          }
        }
      }
      else {
        self.$each.$$p = function() {
          var value = $scope.get('Opal').$destructure(arguments);

          if ((($a = value) !== nil && (!$a.$$is_boolean || $a == true))) {
            if (result === true) {
              result = false;
              return $breaker;
            }

            result = true;
          }
        }
      }

      self.$each();

      return result;
    
    });

    Opal.defn(self, '$partition', TMP_30 = function() {
      var $a, self = this, $iter = TMP_30.$$p, block = $iter || nil;

      TMP_30.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("partition")
      };
      
      var truthy = [], falsy = [];

      self.$each.$$p = function() {
        var param = $scope.get('Opal').$destructure(arguments),
            value = Opal.yield1(block, param);

        if (value === $breaker) {
          result = $breaker.$v;
          return $breaker;
        }

        if ((($a = value) !== nil && (!$a.$$is_boolean || $a == true))) {
          truthy.push(param);
        }
        else {
          falsy.push(param);
        }
      };

      self.$each();

      return [truthy, falsy];
    
    });

    Opal.defn(self, '$reduce', def.$inject);

    Opal.defn(self, '$reject', TMP_31 = function() {
      var $a, self = this, $iter = TMP_31.$$p, block = $iter || nil;

      TMP_31.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("reject")
      };
      
      var result = [];

      self.$each.$$p = function() {
        var param = $scope.get('Opal').$destructure(arguments),
            value = Opal.yield1(block, param);

        if (value === $breaker) {
          result = $breaker.$v;
          return $breaker;
        }

        if ((($a = value) === nil || ($a.$$is_boolean && $a == false))) {
          result.push(param);
        }
      };

      self.$each();

      return result;
    
    });

    Opal.defn(self, '$reverse_each', TMP_32 = function() {
      var self = this, $iter = TMP_32.$$p, block = $iter || nil;

      TMP_32.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("reverse_each")
      };
      
      var result = [];

      self.$each.$$p = function() {
        result.push(arguments);
      };

      self.$each();

      for (var i = result.length - 1; i >= 0; i--) {
        Opal.yieldX(block, result[i]);
      }

      return result;
    
    });

    Opal.defn(self, '$select', def.$find_all);

    Opal.defn(self, '$slice_before', TMP_33 = function(pattern) {
      var $a, $b, TMP_34, self = this, $iter = TMP_33.$$p, block = $iter || nil;

      TMP_33.$$p = null;
      if ((($a = pattern === undefined && block === nil || arguments.length > 1) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('ArgumentError'), "wrong number of arguments (" + (arguments.length) + " for 1)")};
      return ($a = ($b = $scope.get('Enumerator')).$new, $a.$$p = (TMP_34 = function(e){var self = TMP_34.$$s || this, $a;
if (e == null) e = nil;
      
        var slice = [];

        if (block !== nil) {
          if (pattern === undefined) {
            self.$each.$$p = function() {
              var param = $scope.get('Opal').$destructure(arguments),
                  value = Opal.yield1(block, param);

              if ((($a = value) !== nil && (!$a.$$is_boolean || $a == true)) && slice.length > 0) {
                e['$<<'](slice);
                slice = [];
              }

              slice.push(param);
            };
          }
          else {
            self.$each.$$p = function() {
              var param = $scope.get('Opal').$destructure(arguments),
                  value = block(param, pattern.$dup());

              if ((($a = value) !== nil && (!$a.$$is_boolean || $a == true)) && slice.length > 0) {
                e['$<<'](slice);
                slice = [];
              }

              slice.push(param);
            };
          }
        }
        else {
          self.$each.$$p = function() {
            var param = $scope.get('Opal').$destructure(arguments),
                value = pattern['$==='](param);

            if ((($a = value) !== nil && (!$a.$$is_boolean || $a == true)) && slice.length > 0) {
              e['$<<'](slice);
              slice = [];
            }

            slice.push(param);
          };
        }

        self.$each();

        if (slice.length > 0) {
          e['$<<'](slice);
        }
      ;}, TMP_34.$$s = self, TMP_34), $a).call($b);
    });

    Opal.defn(self, '$sort', TMP_35 = function() {
      var $a, $b, TMP_36, self = this, $iter = TMP_35.$$p, block = $iter || nil, ary = nil;

      TMP_35.$$p = null;
      ary = self.$to_a();
      if ((block !== nil)) {
        } else {
        block = ($a = ($b = self).$lambda, $a.$$p = (TMP_36 = function(a, b){var self = TMP_36.$$s || this;
if (a == null) a = nil;if (b == null) b = nil;
        return a['$<=>'](b)}, TMP_36.$$s = self, TMP_36), $a).call($b)
      };
      return ary.sort(block);
    });

    Opal.defn(self, '$sort_by', TMP_37 = function() {
      var $a, $b, TMP_38, $c, $d, TMP_39, $e, $f, TMP_40, self = this, $iter = TMP_37.$$p, block = $iter || nil;

      TMP_37.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("sort_by")
      };
      return ($a = ($b = ($c = ($d = ($e = ($f = self).$map, $e.$$p = (TMP_40 = function(){var self = TMP_40.$$s || this;

      arg = $scope.get('Opal').$destructure(arguments);
        return [block.$call(arg), arg];}, TMP_40.$$s = self, TMP_40), $e).call($f)).$sort, $c.$$p = (TMP_39 = function(a, b){var self = TMP_39.$$s || this;
if (a == null) a = nil;if (b == null) b = nil;
      return a['$[]'](0)['$<=>'](b['$[]'](0))}, TMP_39.$$s = self, TMP_39), $c).call($d)).$map, $a.$$p = (TMP_38 = function(arg){var self = TMP_38.$$s || this;
if (arg == null) arg = nil;
      return arg[1];}, TMP_38.$$s = self, TMP_38), $a).call($b);
    });

    Opal.defn(self, '$take', function(num) {
      var self = this;

      return self.$first(num);
    });

    Opal.defn(self, '$take_while', TMP_41 = function() {
      var $a, self = this, $iter = TMP_41.$$p, block = $iter || nil;

      TMP_41.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        return self.$enum_for("take_while")
      };
      
      var result = [];

      self.$each.$$p = function() {
        var param = $scope.get('Opal').$destructure(arguments),
            value = Opal.yield1(block, param);

        if (value === $breaker) {
          result = $breaker.$v;
          return $breaker;
        }

        if ((($a = value) === nil || ($a.$$is_boolean && $a == false))) {
          return $breaker;
        }

        result.push(param);
      };

      self.$each();

      return result;
    
    });

    Opal.defn(self, '$to_a', def.$entries);

    Opal.defn(self, '$zip', TMP_42 = function(others) {
      var $a, self = this, $iter = TMP_42.$$p, block = $iter || nil;

      others = $slice.call(arguments, 0);
      TMP_42.$$p = null;
      return ($a = self.$to_a()).$zip.apply($a, [].concat(others));
    });
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["corelib/enumerator"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  Opal.add_stubs(['$require', '$include', '$allocate', '$new', '$to_proc', '$coerce_to', '$nil?', '$empty?', '$+', '$class', '$__send__', '$===', '$call', '$enum_for', '$destructure', '$inspect', '$[]', '$raise', '$yield', '$each', '$enumerator_size', '$respond_to?', '$try_convert', '$<', '$for']);
  self.$require("corelib/enumerable");
  return (function($base, $super) {
    function $Enumerator(){};
    var self = $Enumerator = $klass($base, $super, 'Enumerator', $Enumerator);

    var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_2, TMP_3, TMP_4;

    def.size = def.args = def.object = def.method = nil;
    self.$include($scope.get('Enumerable'));

    Opal.defs(self, '$for', TMP_1 = function(object, method, args) {
      var self = this, $iter = TMP_1.$$p, block = $iter || nil;

      args = $slice.call(arguments, 2);
      if (method == null) {
        method = "each"
      }
      TMP_1.$$p = null;
      
      var obj = self.$allocate();

      obj.object = object;
      obj.size   = block;
      obj.method = method;
      obj.args   = args;

      return obj;
    ;
    });

    def.$initialize = TMP_2 = function() {
      var $a, $b, self = this, $iter = TMP_2.$$p, block = $iter || nil;

      TMP_2.$$p = null;
      if (block !== false && block !== nil) {
        self.object = ($a = ($b = $scope.get('Generator')).$new, $a.$$p = block.$to_proc(), $a).call($b);
        self.method = "each";
        self.args = [];
        self.size = arguments[0] || nil;
        if ((($a = self.size) !== nil && (!$a.$$is_boolean || $a == true))) {
          return self.size = $scope.get('Opal').$coerce_to(self.size, $scope.get('Integer'), "to_int")
          } else {
          return nil
        };
        } else {
        self.object = arguments[0];
        self.method = arguments[1] || "each";
        self.args = $slice.call(arguments, 2);
        return self.size = nil;
      };
    };

    def.$each = TMP_3 = function(args) {
      var $a, $b, $c, self = this, $iter = TMP_3.$$p, block = $iter || nil;

      args = $slice.call(arguments, 0);
      TMP_3.$$p = null;
      if ((($a = ($b = block['$nil?'](), $b !== false && $b !== nil ?args['$empty?']() : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self};
      args = self.args['$+'](args);
      if ((($a = block['$nil?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
        return ($a = self.$class()).$new.apply($a, [self.object, self.method].concat(args))};
      return ($b = ($c = self.object).$__send__, $b.$$p = block.$to_proc(), $b).apply($c, [self.method].concat(args));
    };

    def.$size = function() {
      var $a, self = this;

      if ((($a = $scope.get('Proc')['$==='](self.size)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return ($a = self.size).$call.apply($a, [].concat(self.args))
        } else {
        return self.size
      };
    };

    def.$with_index = TMP_4 = function(offset) {
      var self = this, $iter = TMP_4.$$p, block = $iter || nil;

      if (offset == null) {
        offset = 0
      }
      TMP_4.$$p = null;
      if (offset !== false && offset !== nil) {
        offset = $scope.get('Opal').$coerce_to(offset, $scope.get('Integer'), "to_int")
        } else {
        offset = 0
      };
      if (block !== false && block !== nil) {
        } else {
        return self.$enum_for("with_index", offset)
      };
      
      var result, index = 0;

      self.$each.$$p = function() {
        var param = $scope.get('Opal').$destructure(arguments),
            value = block(param, index);

        if (value === $breaker) {
          result = $breaker.$v;
          return $breaker;
        }

        index++;
      }

      self.$each();

      if (result !== undefined) {
        return result;
      }

      return nil;
    
    };

    Opal.defn(self, '$with_object', def.$each_with_object);

    def.$inspect = function() {
      var $a, self = this, result = nil;

      result = "#<" + (self.$class()) + ": " + (self.object.$inspect()) + ":" + (self.method);
      if ((($a = self.args['$empty?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        result = result['$+']("(" + (self.args.$inspect()['$[]']($scope.get('Range').$new(1, -2))) + ")")
      };
      return result['$+'](">");
    };

    (function($base, $super) {
      function $Generator(){};
      var self = $Generator = $klass($base, $super, 'Generator', $Generator);

      var def = self.$$proto, $scope = self.$$scope, TMP_5, TMP_6;

      def.block = nil;
      self.$include($scope.get('Enumerable'));

      def.$initialize = TMP_5 = function() {
        var self = this, $iter = TMP_5.$$p, block = $iter || nil;

        TMP_5.$$p = null;
        if (block !== false && block !== nil) {
          } else {
          self.$raise($scope.get('LocalJumpError'), "no block given")
        };
        return self.block = block;
      };

      return (def.$each = TMP_6 = function(args) {
        var $a, $b, self = this, $iter = TMP_6.$$p, block = $iter || nil, yielder = nil;

        args = $slice.call(arguments, 0);
        TMP_6.$$p = null;
        yielder = ($a = ($b = $scope.get('Yielder')).$new, $a.$$p = block.$to_proc(), $a).call($b);
        
        try {
          args.unshift(yielder);

          if (Opal.yieldX(self.block, args) === $breaker) {
            return $breaker.$v;
          }
        }
        catch (e) {
          if (e === $breaker) {
            return $breaker.$v;
          }
          else {
            throw e;
          }
        }
      ;
        return self;
      }, nil) && 'each';
    })(self, null);

    (function($base, $super) {
      function $Yielder(){};
      var self = $Yielder = $klass($base, $super, 'Yielder', $Yielder);

      var def = self.$$proto, $scope = self.$$scope, TMP_7;

      def.block = nil;
      def.$initialize = TMP_7 = function() {
        var self = this, $iter = TMP_7.$$p, block = $iter || nil;

        TMP_7.$$p = null;
        return self.block = block;
      };

      def.$yield = function(values) {
        var self = this;

        values = $slice.call(arguments, 0);
        
        var value = Opal.yieldX(self.block, values);

        if (value === $breaker) {
          throw $breaker;
        }

        return value;
      ;
      };

      return (def['$<<'] = function(values) {
        var $a, self = this;

        values = $slice.call(arguments, 0);
        ($a = self).$yield.apply($a, [].concat(values));
        return self;
      }, nil) && '<<';
    })(self, null);

    return (function($base, $super) {
      function $Lazy(){};
      var self = $Lazy = $klass($base, $super, 'Lazy', $Lazy);

      var def = self.$$proto, $scope = self.$$scope, TMP_8, TMP_11, TMP_13, TMP_18, TMP_20, TMP_21, TMP_23, TMP_26, TMP_29;

      def.enumerator = nil;
      (function($base, $super) {
        function $StopLazyError(){};
        var self = $StopLazyError = $klass($base, $super, 'StopLazyError', $StopLazyError);

        var def = self.$$proto, $scope = self.$$scope;

        return nil;
      })(self, $scope.get('Exception'));

      def.$initialize = TMP_8 = function(object, size) {
        var TMP_9, self = this, $iter = TMP_8.$$p, block = $iter || nil;

        if (size == null) {
          size = nil
        }
        TMP_8.$$p = null;
        if ((block !== nil)) {
          } else {
          self.$raise($scope.get('ArgumentError'), "tried to call lazy new without a block")
        };
        self.enumerator = object;
        return Opal.find_super_dispatcher(self, 'initialize', TMP_8, (TMP_9 = function(yielder, each_args){var self = TMP_9.$$s || this, $a, $b, TMP_10;
if (yielder == null) yielder = nil;each_args = $slice.call(arguments, 1);
        try {
          return ($a = ($b = object).$each, $a.$$p = (TMP_10 = function(args){var self = TMP_10.$$s || this;
args = $slice.call(arguments, 0);
            
              args.unshift(yielder);

              if (Opal.yieldX(block, args) === $breaker) {
                return $breaker;
              }
            ;}, TMP_10.$$s = self, TMP_10), $a).apply($b, [].concat(each_args))
          } catch ($err) {if (Opal.rescue($err, [$scope.get('Exception')])) {
            return nil
            }else { throw $err; }
          }}, TMP_9.$$s = self, TMP_9)).apply(self, [size]);
      };

      Opal.defn(self, '$force', def.$to_a);

      def.$lazy = function() {
        var self = this;

        return self;
      };

      def.$collect = TMP_11 = function() {
        var $a, $b, TMP_12, self = this, $iter = TMP_11.$$p, block = $iter || nil;

        TMP_11.$$p = null;
        if (block !== false && block !== nil) {
          } else {
          self.$raise($scope.get('ArgumentError'), "tried to call lazy map without a block")
        };
        return ($a = ($b = $scope.get('Lazy')).$new, $a.$$p = (TMP_12 = function(enum$, args){var self = TMP_12.$$s || this;
if (enum$ == null) enum$ = nil;args = $slice.call(arguments, 1);
        
          var value = Opal.yieldX(block, args);

          if (value === $breaker) {
            return $breaker;
          }

          enum$.$yield(value);
        }, TMP_12.$$s = self, TMP_12), $a).call($b, self, self.$enumerator_size());
      };

      def.$collect_concat = TMP_13 = function() {
        var $a, $b, TMP_14, self = this, $iter = TMP_13.$$p, block = $iter || nil;

        TMP_13.$$p = null;
        if (block !== false && block !== nil) {
          } else {
          self.$raise($scope.get('ArgumentError'), "tried to call lazy map without a block")
        };
        return ($a = ($b = $scope.get('Lazy')).$new, $a.$$p = (TMP_14 = function(enum$, args){var self = TMP_14.$$s || this, $a, $b, TMP_15, $c, TMP_16;
if (enum$ == null) enum$ = nil;args = $slice.call(arguments, 1);
        
          var value = Opal.yieldX(block, args);

          if (value === $breaker) {
            return $breaker;
          }

          if ((value)['$respond_to?']("force") && (value)['$respond_to?']("each")) {
            ($a = ($b = (value)).$each, $a.$$p = (TMP_15 = function(v){var self = TMP_15.$$s || this;
if (v == null) v = nil;
          return enum$.$yield(v)}, TMP_15.$$s = self, TMP_15), $a).call($b)
          }
          else {
            var array = $scope.get('Opal').$try_convert(value, $scope.get('Array'), "to_ary");

            if (array === nil) {
              enum$.$yield(value);
            }
            else {
              ($a = ($c = (value)).$each, $a.$$p = (TMP_16 = function(v){var self = TMP_16.$$s || this;
if (v == null) v = nil;
          return enum$.$yield(v)}, TMP_16.$$s = self, TMP_16), $a).call($c);
            }
          }
        ;}, TMP_14.$$s = self, TMP_14), $a).call($b, self, nil);
      };

      def.$drop = function(n) {
        var $a, $b, TMP_17, self = this, current_size = nil, set_size = nil, dropped = nil;

        n = $scope.get('Opal').$coerce_to(n, $scope.get('Integer'), "to_int");
        if (n['$<'](0)) {
          self.$raise($scope.get('ArgumentError'), "attempt to drop negative size")};
        current_size = self.$enumerator_size();
        set_size = (function() {if ((($a = $scope.get('Integer')['$==='](current_size)) !== nil && (!$a.$$is_boolean || $a == true))) {
          if (n['$<'](current_size)) {
            return n
            } else {
            return current_size
          }
          } else {
          return current_size
        }; return nil; })();
        dropped = 0;
        return ($a = ($b = $scope.get('Lazy')).$new, $a.$$p = (TMP_17 = function(enum$, args){var self = TMP_17.$$s || this, $a;
if (enum$ == null) enum$ = nil;args = $slice.call(arguments, 1);
        if (dropped['$<'](n)) {
            return dropped = dropped['$+'](1)
            } else {
            return ($a = enum$).$yield.apply($a, [].concat(args))
          }}, TMP_17.$$s = self, TMP_17), $a).call($b, self, set_size);
      };

      def.$drop_while = TMP_18 = function() {
        var $a, $b, TMP_19, self = this, $iter = TMP_18.$$p, block = $iter || nil, succeeding = nil;

        TMP_18.$$p = null;
        if (block !== false && block !== nil) {
          } else {
          self.$raise($scope.get('ArgumentError'), "tried to call lazy drop_while without a block")
        };
        succeeding = true;
        return ($a = ($b = $scope.get('Lazy')).$new, $a.$$p = (TMP_19 = function(enum$, args){var self = TMP_19.$$s || this, $a, $b;
if (enum$ == null) enum$ = nil;args = $slice.call(arguments, 1);
        if (succeeding !== false && succeeding !== nil) {
            
            var value = Opal.yieldX(block, args);

            if (value === $breaker) {
              return $breaker;
            }

            if ((($a = value) === nil || ($a.$$is_boolean && $a == false))) {
              succeeding = false;

              ($a = enum$).$yield.apply($a, [].concat(args));
            }
          
            } else {
            return ($b = enum$).$yield.apply($b, [].concat(args))
          }}, TMP_19.$$s = self, TMP_19), $a).call($b, self, nil);
      };

      def.$enum_for = TMP_20 = function(method, args) {
        var $a, $b, self = this, $iter = TMP_20.$$p, block = $iter || nil;

        args = $slice.call(arguments, 1);
        if (method == null) {
          method = "each"
        }
        TMP_20.$$p = null;
        return ($a = ($b = self.$class()).$for, $a.$$p = block.$to_proc(), $a).apply($b, [self, method].concat(args));
      };

      def.$find_all = TMP_21 = function() {
        var $a, $b, TMP_22, self = this, $iter = TMP_21.$$p, block = $iter || nil;

        TMP_21.$$p = null;
        if (block !== false && block !== nil) {
          } else {
          self.$raise($scope.get('ArgumentError'), "tried to call lazy select without a block")
        };
        return ($a = ($b = $scope.get('Lazy')).$new, $a.$$p = (TMP_22 = function(enum$, args){var self = TMP_22.$$s || this, $a;
if (enum$ == null) enum$ = nil;args = $slice.call(arguments, 1);
        
          var value = Opal.yieldX(block, args);

          if (value === $breaker) {
            return $breaker;
          }

          if ((($a = value) !== nil && (!$a.$$is_boolean || $a == true))) {
            ($a = enum$).$yield.apply($a, [].concat(args));
          }
        ;}, TMP_22.$$s = self, TMP_22), $a).call($b, self, nil);
      };

      Opal.defn(self, '$flat_map', def.$collect_concat);

      def.$grep = TMP_23 = function(pattern) {
        var $a, $b, TMP_24, $c, TMP_25, self = this, $iter = TMP_23.$$p, block = $iter || nil;

        TMP_23.$$p = null;
        if (block !== false && block !== nil) {
          return ($a = ($b = $scope.get('Lazy')).$new, $a.$$p = (TMP_24 = function(enum$, args){var self = TMP_24.$$s || this, $a;
if (enum$ == null) enum$ = nil;args = $slice.call(arguments, 1);
          
            var param = $scope.get('Opal').$destructure(args),
                value = pattern['$==='](param);

            if ((($a = value) !== nil && (!$a.$$is_boolean || $a == true))) {
              value = Opal.yield1(block, param);

              if (value === $breaker) {
                return $breaker;
              }

              enum$.$yield(Opal.yield1(block, param));
            }
          ;}, TMP_24.$$s = self, TMP_24), $a).call($b, self, nil)
          } else {
          return ($a = ($c = $scope.get('Lazy')).$new, $a.$$p = (TMP_25 = function(enum$, args){var self = TMP_25.$$s || this, $a;
if (enum$ == null) enum$ = nil;args = $slice.call(arguments, 1);
          
            var param = $scope.get('Opal').$destructure(args),
                value = pattern['$==='](param);

            if ((($a = value) !== nil && (!$a.$$is_boolean || $a == true))) {
              enum$.$yield(param);
            }
          ;}, TMP_25.$$s = self, TMP_25), $a).call($c, self, nil)
        };
      };

      Opal.defn(self, '$map', def.$collect);

      Opal.defn(self, '$select', def.$find_all);

      def.$reject = TMP_26 = function() {
        var $a, $b, TMP_27, self = this, $iter = TMP_26.$$p, block = $iter || nil;

        TMP_26.$$p = null;
        if (block !== false && block !== nil) {
          } else {
          self.$raise($scope.get('ArgumentError'), "tried to call lazy reject without a block")
        };
        return ($a = ($b = $scope.get('Lazy')).$new, $a.$$p = (TMP_27 = function(enum$, args){var self = TMP_27.$$s || this, $a;
if (enum$ == null) enum$ = nil;args = $slice.call(arguments, 1);
        
          var value = Opal.yieldX(block, args);

          if (value === $breaker) {
            return $breaker;
          }

          if ((($a = value) === nil || ($a.$$is_boolean && $a == false))) {
            ($a = enum$).$yield.apply($a, [].concat(args));
          }
        ;}, TMP_27.$$s = self, TMP_27), $a).call($b, self, nil);
      };

      def.$take = function(n) {
        var $a, $b, TMP_28, self = this, current_size = nil, set_size = nil, taken = nil;

        n = $scope.get('Opal').$coerce_to(n, $scope.get('Integer'), "to_int");
        if (n['$<'](0)) {
          self.$raise($scope.get('ArgumentError'), "attempt to take negative size")};
        current_size = self.$enumerator_size();
        set_size = (function() {if ((($a = $scope.get('Integer')['$==='](current_size)) !== nil && (!$a.$$is_boolean || $a == true))) {
          if (n['$<'](current_size)) {
            return n
            } else {
            return current_size
          }
          } else {
          return current_size
        }; return nil; })();
        taken = 0;
        return ($a = ($b = $scope.get('Lazy')).$new, $a.$$p = (TMP_28 = function(enum$, args){var self = TMP_28.$$s || this, $a;
if (enum$ == null) enum$ = nil;args = $slice.call(arguments, 1);
        if (taken['$<'](n)) {
            ($a = enum$).$yield.apply($a, [].concat(args));
            return taken = taken['$+'](1);
            } else {
            return self.$raise($scope.get('StopLazyError'))
          }}, TMP_28.$$s = self, TMP_28), $a).call($b, self, set_size);
      };

      def.$take_while = TMP_29 = function() {
        var $a, $b, TMP_30, self = this, $iter = TMP_29.$$p, block = $iter || nil;

        TMP_29.$$p = null;
        if (block !== false && block !== nil) {
          } else {
          self.$raise($scope.get('ArgumentError'), "tried to call lazy take_while without a block")
        };
        return ($a = ($b = $scope.get('Lazy')).$new, $a.$$p = (TMP_30 = function(enum$, args){var self = TMP_30.$$s || this, $a;
if (enum$ == null) enum$ = nil;args = $slice.call(arguments, 1);
        
          var value = Opal.yieldX(block, args);

          if (value === $breaker) {
            return $breaker;
          }

          if ((($a = value) !== nil && (!$a.$$is_boolean || $a == true))) {
            ($a = enum$).$yield.apply($a, [].concat(args));
          }
          else {
            self.$raise($scope.get('StopLazyError'));
          }
        ;}, TMP_30.$$s = self, TMP_30), $a).call($b, self, nil);
      };

      Opal.defn(self, '$to_enum', def.$enum_for);

      return (def.$inspect = function() {
        var self = this;

        return "#<" + (self.$class()) + ": " + (self.enumerator.$inspect()) + ">";
      }, nil) && 'inspect';
    })(self, self);
  })(self, null);
};

/* Generated by Opal 0.7.2 */
Opal.modules["corelib/array"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass, $gvars = Opal.gvars, $range = Opal.range;

  Opal.add_stubs(['$require', '$include', '$new', '$class', '$raise', '$===', '$to_a', '$respond_to?', '$to_ary', '$coerce_to', '$coerce_to?', '$==', '$to_str', '$clone', '$hash', '$<=>', '$inspect', '$empty?', '$enum_for', '$nil?', '$coerce_to!', '$initialize_clone', '$initialize_dup', '$replace', '$eql?', '$length', '$begin', '$end', '$exclude_end?', '$flatten', '$__id__', '$[]', '$to_s', '$join', '$delete_if', '$to_proc', '$each', '$reverse', '$!', '$map', '$rand', '$keep_if', '$shuffle!', '$>', '$<', '$sort', '$times', '$[]=', '$<<', '$at']);
  self.$require("corelib/enumerable");
  return (function($base, $super) {
    function $Array(){};
    var self = $Array = $klass($base, $super, 'Array', $Array);

    var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_2, TMP_3, TMP_4, TMP_5, TMP_6, TMP_7, TMP_8, TMP_9, TMP_10, TMP_11, TMP_12, TMP_13, TMP_14, TMP_15, TMP_17, TMP_18, TMP_19, TMP_20, TMP_21, TMP_24;

    def.length = nil;
    self.$include($scope.get('Enumerable'));

    def.$$is_array = true;

    Opal.defs(self, '$[]', function(objects) {
      var self = this;

      objects = $slice.call(arguments, 0);
      return objects;
    });

    def.$initialize = function(args) {
      var $a, self = this;

      args = $slice.call(arguments, 0);
      return ($a = self.$class()).$new.apply($a, [].concat(args));
    };

    Opal.defs(self, '$new', TMP_1 = function(size, obj) {
      var $a, self = this, $iter = TMP_1.$$p, block = $iter || nil;

      if (size == null) {
        size = nil
      }
      if (obj == null) {
        obj = nil
      }
      TMP_1.$$p = null;
      if ((($a = arguments.length > 2) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('ArgumentError'), "wrong number of arguments (" + (arguments.length) + " for 0..2)")};
      if ((($a = arguments.length === 0) !== nil && (!$a.$$is_boolean || $a == true))) {
        return []};
      if ((($a = arguments.length === 1) !== nil && (!$a.$$is_boolean || $a == true))) {
        if ((($a = $scope.get('Array')['$==='](size)) !== nil && (!$a.$$is_boolean || $a == true))) {
          return size.$to_a()
        } else if ((($a = size['$respond_to?']("to_ary")) !== nil && (!$a.$$is_boolean || $a == true))) {
          return size.$to_ary()}};
      size = $scope.get('Opal').$coerce_to(size, $scope.get('Integer'), "to_int");
      if ((($a = size < 0) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('ArgumentError'), "negative array size")};
      
      var result = [];

      if (block === nil) {
        for (var i = 0; i < size; i++) {
          result.push(obj);
        }
      }
      else {
        for (var i = 0, value; i < size; i++) {
          value = block(i);

          if (value === $breaker) {
            return $breaker.$v;
          }

          result[i] = value;
        }
      }

      return result;
    
    });

    Opal.defs(self, '$try_convert', function(obj) {
      var self = this;

      return $scope.get('Opal')['$coerce_to?'](obj, $scope.get('Array'), "to_ary");
    });

    def['$&'] = function(other) {
      var $a, self = this;

      if ((($a = $scope.get('Array')['$==='](other)) !== nil && (!$a.$$is_boolean || $a == true))) {
        other = other.$to_a()
        } else {
        other = $scope.get('Opal').$coerce_to(other, $scope.get('Array'), "to_ary").$to_a()
      };
      
      var result = [],
          seen   = {};

      for (var i = 0, length = self.length; i < length; i++) {
        var item = self[i];

        if (!seen[item]) {
          for (var j = 0, length2 = other.length; j < length2; j++) {
            var item2 = other[j];

            if (!seen[item2] && (item)['$=='](item2)) {
              seen[item] = true;
              result.push(item);
            }
          }
        }
      }

      return result;
    
    };

    def['$|'] = function(other) {
      var $a, self = this;

      if ((($a = $scope.get('Array')['$==='](other)) !== nil && (!$a.$$is_boolean || $a == true))) {
        other = other.$to_a()
        } else {
        other = $scope.get('Opal').$coerce_to(other, $scope.get('Array'), "to_ary").$to_a()
      };
      
      var result = [],
          seen   = {};

      for (var i = 0, length = self.length; i < length; i++) {
        var item = self[i];

        if (!seen[item]) {
          seen[item] = true;
          result.push(item);
        }
      }

      for (var i = 0, length = other.length; i < length; i++) {
        var item = other[i];

        if (!seen[item]) {
          seen[item] = true;
          result.push(item);
        }
      }
      return result;
    
    };

    def['$*'] = function(other) {
      var $a, self = this;

      if ((($a = other['$respond_to?']("to_str")) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self.join(other.$to_str())};
      if ((($a = other['$respond_to?']("to_int")) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise($scope.get('TypeError'), "no implicit conversion of " + (other.$class()) + " into Integer")
      };
      other = $scope.get('Opal').$coerce_to(other, $scope.get('Integer'), "to_int");
      if ((($a = other < 0) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('ArgumentError'), "negative argument")};
      
      var result = [];

      for (var i = 0; i < other; i++) {
        result = result.concat(self);
      }

      return result;
    
    };

    def['$+'] = function(other) {
      var $a, self = this;

      if ((($a = $scope.get('Array')['$==='](other)) !== nil && (!$a.$$is_boolean || $a == true))) {
        other = other.$to_a()
        } else {
        other = $scope.get('Opal').$coerce_to(other, $scope.get('Array'), "to_ary").$to_a()
      };
      return self.concat(other);
    };

    def['$-'] = function(other) {
      var $a, self = this;

      if ((($a = $scope.get('Array')['$==='](other)) !== nil && (!$a.$$is_boolean || $a == true))) {
        other = other.$to_a()
        } else {
        other = $scope.get('Opal').$coerce_to(other, $scope.get('Array'), "to_ary").$to_a()
      };
      if ((($a = self.length === 0) !== nil && (!$a.$$is_boolean || $a == true))) {
        return []};
      if ((($a = other.length === 0) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self.$clone()};
      
      var seen   = {},
          result = [];

      for (var i = 0, length = other.length; i < length; i++) {
        seen[other[i]] = true;
      }

      for (var i = 0, length = self.length; i < length; i++) {
        var item = self[i];

        if (!seen[item]) {
          result.push(item);
        }
      }

      return result;
    
    };

    def['$<<'] = function(object) {
      var self = this;

      self.push(object);
      return self;
    };

    def['$<=>'] = function(other) {
      var $a, self = this;

      if ((($a = $scope.get('Array')['$==='](other)) !== nil && (!$a.$$is_boolean || $a == true))) {
        other = other.$to_a()
      } else if ((($a = other['$respond_to?']("to_ary")) !== nil && (!$a.$$is_boolean || $a == true))) {
        other = other.$to_ary().$to_a()
        } else {
        return nil
      };
      
      if (self.$hash() === other.$hash()) {
        return 0;
      }

      if (self.length != other.length) {
        return (self.length > other.length) ? 1 : -1;
      }

      for (var i = 0, length = self.length; i < length; i++) {
        var tmp = (self[i])['$<=>'](other[i]);

        if (tmp !== 0) {
          return tmp;
        }
      }

      return 0;
    ;
    };

    def['$=='] = function(other) {
      var $a, self = this;

      if ((($a = self === other) !== nil && (!$a.$$is_boolean || $a == true))) {
        return true};
      if ((($a = $scope.get('Array')['$==='](other)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        if ((($a = other['$respond_to?']("to_ary")) !== nil && (!$a.$$is_boolean || $a == true))) {
          } else {
          return false
        };
        return other['$=='](self);
      };
      other = other.$to_a();
      if ((($a = self.length === other.length) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        return false
      };
      
      for (var i = 0, length = self.length; i < length; i++) {
        var a = self[i],
            b = other[i];

        if (a.$$is_array && b.$$is_array && (a === self)) {
          continue;
        }

        if (!(a)['$=='](b)) {
          return false;
        }
      }
    
      return true;
    };

    def['$[]'] = function(index, length) {
      var $a, self = this;

      if ((($a = $scope.get('Range')['$==='](index)) !== nil && (!$a.$$is_boolean || $a == true))) {
        
        var size    = self.length,
            exclude = index.exclude,
            from    = $scope.get('Opal').$coerce_to(index.begin, $scope.get('Integer'), "to_int"),
            to      = $scope.get('Opal').$coerce_to(index.end, $scope.get('Integer'), "to_int");

        if (from < 0) {
          from += size;

          if (from < 0) {
            return nil;
          }
        }

        if (from > size) {
          return nil;
        }

        if (to < 0) {
          to += size;

          if (to < 0) {
            return [];
          }
        }

        if (!exclude) {
          to += 1;
        }

        return self.slice(from, to);
      ;
        } else {
        index = $scope.get('Opal').$coerce_to(index, $scope.get('Integer'), "to_int");
        
        var size = self.length;

        if (index < 0) {
          index += size;

          if (index < 0) {
            return nil;
          }
        }

        if (length === undefined) {
          if (index >= size || index < 0) {
            return nil;
          }

          return self[index];
        }
        else {
          length = $scope.get('Opal').$coerce_to(length, $scope.get('Integer'), "to_int");

          if (length < 0 || index > size || index < 0) {
            return nil;
          }

          return self.slice(index, index + length);
        }
      
      };
    };

    def['$[]='] = function(index, value, extra) {
      var $a, self = this, data = nil, length = nil;

      if ((($a = $scope.get('Range')['$==='](index)) !== nil && (!$a.$$is_boolean || $a == true))) {
        if ((($a = $scope.get('Array')['$==='](value)) !== nil && (!$a.$$is_boolean || $a == true))) {
          data = value.$to_a()
        } else if ((($a = value['$respond_to?']("to_ary")) !== nil && (!$a.$$is_boolean || $a == true))) {
          data = value.$to_ary().$to_a()
          } else {
          data = [value]
        };
        
        var size    = self.length,
            exclude = index.exclude,
            from    = $scope.get('Opal').$coerce_to(index.begin, $scope.get('Integer'), "to_int"),
            to      = $scope.get('Opal').$coerce_to(index.end, $scope.get('Integer'), "to_int");

        if (from < 0) {
          from += size;

          if (from < 0) {
            self.$raise($scope.get('RangeError'), "" + (index.$inspect()) + " out of range");
          }
        }

        if (to < 0) {
          to += size;
        }

        if (!exclude) {
          to += 1;
        }

        if (from > size) {
          for (var i = size; i < from; i++) {
            self[i] = nil;
          }
        }

        if (to < 0) {
          self.splice.apply(self, [from, 0].concat(data));
        }
        else {
          self.splice.apply(self, [from, to - from].concat(data));
        }

        return value;
      ;
        } else {
        if ((($a = extra === undefined) !== nil && (!$a.$$is_boolean || $a == true))) {
          length = 1
          } else {
          length = value;
          value = extra;
          if ((($a = $scope.get('Array')['$==='](value)) !== nil && (!$a.$$is_boolean || $a == true))) {
            data = value.$to_a()
          } else if ((($a = value['$respond_to?']("to_ary")) !== nil && (!$a.$$is_boolean || $a == true))) {
            data = value.$to_ary().$to_a()
            } else {
            data = [value]
          };
        };
        
        var size   = self.length,
            index  = $scope.get('Opal').$coerce_to(index, $scope.get('Integer'), "to_int"),
            length = $scope.get('Opal').$coerce_to(length, $scope.get('Integer'), "to_int"),
            old;

        if (index < 0) {
          old    = index;
          index += size;

          if (index < 0) {
            self.$raise($scope.get('IndexError'), "index " + (old) + " too small for array; minimum " + (-self.length));
          }
        }

        if (length < 0) {
          self.$raise($scope.get('IndexError'), "negative length (" + (length) + ")")
        }

        if (index > size) {
          for (var i = size; i < index; i++) {
            self[i] = nil;
          }
        }

        if (extra === undefined) {
          self[index] = value;
        }
        else {
          self.splice.apply(self, [index, length].concat(data));
        }

        return value;
      ;
      };
    };

    def.$assoc = function(object) {
      var self = this;

      
      for (var i = 0, length = self.length, item; i < length; i++) {
        if (item = self[i], item.length && (item[0])['$=='](object)) {
          return item;
        }
      }

      return nil;
    
    };

    def.$at = function(index) {
      var self = this;

      index = $scope.get('Opal').$coerce_to(index, $scope.get('Integer'), "to_int");
      
      if (index < 0) {
        index += self.length;
      }

      if (index < 0 || index >= self.length) {
        return nil;
      }

      return self[index];
    
    };

    def.$cycle = TMP_2 = function(n) {
      var $a, $b, self = this, $iter = TMP_2.$$p, block = $iter || nil;

      if (n == null) {
        n = nil
      }
      TMP_2.$$p = null;
      if ((($a = ((($b = self['$empty?']()) !== false && $b !== nil) ? $b : n['$=='](0))) !== nil && (!$a.$$is_boolean || $a == true))) {
        return nil};
      if (block !== false && block !== nil) {
        } else {
        return self.$enum_for("cycle", n)
      };
      if ((($a = n['$nil?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
        
        while (true) {
          for (var i = 0, length = self.length; i < length; i++) {
            var value = Opal.yield1(block, self[i]);

            if (value === $breaker) {
              return $breaker.$v;
            }
          }
        }
      
        } else {
        n = $scope.get('Opal')['$coerce_to!'](n, $scope.get('Integer'), "to_int");
        
        if (n <= 0) {
          return self;
        }

        while (n > 0) {
          for (var i = 0, length = self.length; i < length; i++) {
            var value = Opal.yield1(block, self[i]);

            if (value === $breaker) {
              return $breaker.$v;
            }
          }

          n--;
        }
      
      };
      return self;
    };

    def.$clear = function() {
      var self = this;

      self.splice(0, self.length);
      return self;
    };

    def.$clone = function() {
      var self = this, copy = nil;

      copy = [];
      copy.$initialize_clone(self);
      return copy;
    };

    def.$dup = function() {
      var self = this, copy = nil;

      copy = [];
      copy.$initialize_dup(self);
      return copy;
    };

    def.$initialize_copy = function(other) {
      var self = this;

      return self.$replace(other);
    };

    def.$collect = TMP_3 = function() {
      var self = this, $iter = TMP_3.$$p, block = $iter || nil;

      TMP_3.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("collect")
      };
      
      var result = [];

      for (var i = 0, length = self.length; i < length; i++) {
        var value = Opal.yield1(block, self[i]);

        if (value === $breaker) {
          return $breaker.$v;
        }

        result.push(value);
      }

      return result;
    
    };

    def['$collect!'] = TMP_4 = function() {
      var self = this, $iter = TMP_4.$$p, block = $iter || nil;

      TMP_4.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("collect!")
      };
      
      for (var i = 0, length = self.length; i < length; i++) {
        var value = Opal.yield1(block, self[i]);

        if (value === $breaker) {
          return $breaker.$v;
        }

        self[i] = value;
      }
    
      return self;
    };

    def.$compact = function() {
      var self = this;

      
      var result = [];

      for (var i = 0, length = self.length, item; i < length; i++) {
        if ((item = self[i]) !== nil) {
          result.push(item);
        }
      }

      return result;
    
    };

    def['$compact!'] = function() {
      var self = this;

      
      var original = self.length;

      for (var i = 0, length = self.length; i < length; i++) {
        if (self[i] === nil) {
          self.splice(i, 1);

          length--;
          i--;
        }
      }

      return self.length === original ? nil : self;
    
    };

    def.$concat = function(other) {
      var $a, self = this;

      if ((($a = $scope.get('Array')['$==='](other)) !== nil && (!$a.$$is_boolean || $a == true))) {
        other = other.$to_a()
        } else {
        other = $scope.get('Opal').$coerce_to(other, $scope.get('Array'), "to_ary").$to_a()
      };
      
      for (var i = 0, length = other.length; i < length; i++) {
        self.push(other[i]);
      }
    
      return self;
    };

    def.$delete = function(object) {
      var self = this;

      
      var original = self.length;

      for (var i = 0, length = original; i < length; i++) {
        if ((self[i])['$=='](object)) {
          self.splice(i, 1);

          length--;
          i--;
        }
      }

      return self.length === original ? nil : object;
    
    };

    def.$delete_at = function(index) {
      var self = this;

      
      index = $scope.get('Opal').$coerce_to(index, $scope.get('Integer'), "to_int");

      if (index < 0) {
        index += self.length;
      }

      if (index < 0 || index >= self.length) {
        return nil;
      }

      var result = self[index];

      self.splice(index, 1);

      return result;
    ;
    };

    def.$delete_if = TMP_5 = function() {
      var self = this, $iter = TMP_5.$$p, block = $iter || nil;

      TMP_5.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("delete_if")
      };
      
      for (var i = 0, length = self.length, value; i < length; i++) {
        if ((value = block(self[i])) === $breaker) {
          return $breaker.$v;
        }

        if (value !== false && value !== nil) {
          self.splice(i, 1);

          length--;
          i--;
        }
      }
    
      return self;
    };

    def.$drop = function(number) {
      var self = this;

      
      if (number < 0) {
        self.$raise($scope.get('ArgumentError'))
      }

      return self.slice(number);
    ;
    };

    Opal.defn(self, '$dup', def.$clone);

    def.$each = TMP_6 = function() {
      var self = this, $iter = TMP_6.$$p, block = $iter || nil;

      TMP_6.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("each")
      };
      
      for (var i = 0, length = self.length; i < length; i++) {
        var value = Opal.yield1(block, self[i]);

        if (value == $breaker) {
          return $breaker.$v;
        }
      }
    
      return self;
    };

    def.$each_index = TMP_7 = function() {
      var self = this, $iter = TMP_7.$$p, block = $iter || nil;

      TMP_7.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("each_index")
      };
      
      for (var i = 0, length = self.length; i < length; i++) {
        var value = Opal.yield1(block, i);

        if (value === $breaker) {
          return $breaker.$v;
        }
      }
    
      return self;
    };

    def['$empty?'] = function() {
      var self = this;

      return self.length === 0;
    };

    def['$eql?'] = function(other) {
      var $a, self = this;

      if ((($a = self === other) !== nil && (!$a.$$is_boolean || $a == true))) {
        return true};
      if ((($a = $scope.get('Array')['$==='](other)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        return false
      };
      other = other.$to_a();
      if ((($a = self.length === other.length) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        return false
      };
      
      for (var i = 0, length = self.length; i < length; i++) {
        var a = self[i],
            b = other[i];

        if (a.$$is_array && b.$$is_array && (a === self)) {
          continue;
        }

        if (!(a)['$eql?'](b)) {
          return false;
        }
      }
    
      return true;
    };

    def.$fetch = TMP_8 = function(index, defaults) {
      var self = this, $iter = TMP_8.$$p, block = $iter || nil;

      TMP_8.$$p = null;
      
      var original = index;

      index = $scope.get('Opal').$coerce_to(index, $scope.get('Integer'), "to_int");

      if (index < 0) {
        index += self.length;
      }

      if (index >= 0 && index < self.length) {
        return self[index];
      }

      if (block !== nil) {
        return block(original);
      }

      if (defaults != null) {
        return defaults;
      }

      if (self.length === 0) {
        self.$raise($scope.get('IndexError'), "index " + (original) + " outside of array bounds: 0...0")
      }
      else {
        self.$raise($scope.get('IndexError'), "index " + (original) + " outside of array bounds: -" + (self.length) + "..." + (self.length));
      }
    ;
    };

    def.$fill = TMP_9 = function(args) {
      var $a, self = this, $iter = TMP_9.$$p, block = $iter || nil, one = nil, two = nil, obj = nil, left = nil, right = nil;

      args = $slice.call(arguments, 0);
      TMP_9.$$p = null;
      if (block !== false && block !== nil) {
        if ((($a = args.length > 2) !== nil && (!$a.$$is_boolean || $a == true))) {
          self.$raise($scope.get('ArgumentError'), "wrong number of arguments (" + (args.$length()) + " for 0..2)")};
        $a = Opal.to_ary(args), one = ($a[0] == null ? nil : $a[0]), two = ($a[1] == null ? nil : $a[1]);
        } else {
        if ((($a = args.length == 0) !== nil && (!$a.$$is_boolean || $a == true))) {
          self.$raise($scope.get('ArgumentError'), "wrong number of arguments (0 for 1..3)")
        } else if ((($a = args.length > 3) !== nil && (!$a.$$is_boolean || $a == true))) {
          self.$raise($scope.get('ArgumentError'), "wrong number of arguments (" + (args.$length()) + " for 1..3)")};
        $a = Opal.to_ary(args), obj = ($a[0] == null ? nil : $a[0]), one = ($a[1] == null ? nil : $a[1]), two = ($a[2] == null ? nil : $a[2]);
      };
      if ((($a = $scope.get('Range')['$==='](one)) !== nil && (!$a.$$is_boolean || $a == true))) {
        if (two !== false && two !== nil) {
          self.$raise($scope.get('TypeError'), "length invalid with range")};
        left = $scope.get('Opal').$coerce_to(one.$begin(), $scope.get('Integer'), "to_int");
        if ((($a = left < 0) !== nil && (!$a.$$is_boolean || $a == true))) {
          left += self.length;};
        if ((($a = left < 0) !== nil && (!$a.$$is_boolean || $a == true))) {
          self.$raise($scope.get('RangeError'), "" + (one.$inspect()) + " out of range")};
        right = $scope.get('Opal').$coerce_to(one.$end(), $scope.get('Integer'), "to_int");
        if ((($a = right < 0) !== nil && (!$a.$$is_boolean || $a == true))) {
          right += self.length;};
        if ((($a = one['$exclude_end?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
          } else {
          right += 1;
        };
        if ((($a = right <= left) !== nil && (!$a.$$is_boolean || $a == true))) {
          return self};
      } else if (one !== false && one !== nil) {
        left = $scope.get('Opal').$coerce_to(one, $scope.get('Integer'), "to_int");
        if ((($a = left < 0) !== nil && (!$a.$$is_boolean || $a == true))) {
          left += self.length;};
        if ((($a = left < 0) !== nil && (!$a.$$is_boolean || $a == true))) {
          left = 0};
        if (two !== false && two !== nil) {
          right = $scope.get('Opal').$coerce_to(two, $scope.get('Integer'), "to_int");
          if ((($a = right == 0) !== nil && (!$a.$$is_boolean || $a == true))) {
            return self};
          right += left;
          } else {
          right = self.length
        };
        } else {
        left = 0;
        right = self.length;
      };
      if ((($a = left > self.length) !== nil && (!$a.$$is_boolean || $a == true))) {
        
        for (var i = self.length; i < right; i++) {
          self[i] = nil;
        }
      ;};
      if ((($a = right > self.length) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.length = right};
      if (block !== false && block !== nil) {
        
        for (var length = self.length; left < right; left++) {
          var value = block(left);

          if (value === $breaker) {
            return $breaker.$v;
          }

          self[left] = value;
        }
      ;
        } else {
        
        for (var length = self.length; left < right; left++) {
          self[left] = obj;
        }
      ;
      };
      return self;
    };

    def.$first = function(count) {
      var self = this;

      
      if (count == null) {
        return self.length === 0 ? nil : self[0];
      }

      count = $scope.get('Opal').$coerce_to(count, $scope.get('Integer'), "to_int");

      if (count < 0) {
        self.$raise($scope.get('ArgumentError'), "negative array size");
      }

      return self.slice(0, count);
    
    };

    def.$flatten = function(level) {
      var self = this;

      
      var result = [];

      for (var i = 0, length = self.length; i < length; i++) {
        var item = self[i];

        if ($scope.get('Opal')['$respond_to?'](item, "to_ary")) {
          item = (item).$to_ary();

          if (level == null) {
            result.push.apply(result, (item).$flatten().$to_a());
          }
          else if (level == 0) {
            result.push(item);
          }
          else {
            result.push.apply(result, (item).$flatten(level - 1).$to_a());
          }
        }
        else {
          result.push(item);
        }
      }

      return result;
    ;
    };

    def['$flatten!'] = function(level) {
      var self = this;

      
      var flattened = self.$flatten(level);

      if (self.length == flattened.length) {
        for (var i = 0, length = self.length; i < length; i++) {
          if (self[i] !== flattened[i]) {
            break;
          }
        }

        if (i == length) {
          return nil;
        }
      }

      self.$replace(flattened);
    ;
      return self;
    };

    def.$hash = function() {
      var self = this;

      
      var hash = ['A'], item, item_hash;
      for (var i = 0, length = self.length; i < length; i++) {
        item = self[i];
        // Guard against recursion
        item_hash = self === item ? 'self' : item.$hash();
        hash.push(item_hash);
      }
      return hash.join(',');
    
    };

    def['$include?'] = function(member) {
      var self = this;

      
      for (var i = 0, length = self.length; i < length; i++) {
        if ((self[i])['$=='](member)) {
          return true;
        }
      }

      return false;
    
    };

    def.$index = TMP_10 = function(object) {
      var self = this, $iter = TMP_10.$$p, block = $iter || nil;

      TMP_10.$$p = null;
      
      if (object != null) {
        for (var i = 0, length = self.length; i < length; i++) {
          if ((self[i])['$=='](object)) {
            return i;
          }
        }
      }
      else if (block !== nil) {
        for (var i = 0, length = self.length, value; i < length; i++) {
          if ((value = block(self[i])) === $breaker) {
            return $breaker.$v;
          }

          if (value !== false && value !== nil) {
            return i;
          }
        }
      }
      else {
        return self.$enum_for("index");
      }

      return nil;
    
    };

    def.$insert = function(index, objects) {
      var self = this;

      objects = $slice.call(arguments, 1);
      
      index = $scope.get('Opal').$coerce_to(index, $scope.get('Integer'), "to_int");

      if (objects.length > 0) {
        if (index < 0) {
          index += self.length + 1;

          if (index < 0) {
            self.$raise($scope.get('IndexError'), "" + (index) + " is out of bounds");
          }
        }
        if (index > self.length) {
          for (var i = self.length; i < index; i++) {
            self.push(nil);
          }
        }

        self.splice.apply(self, [index, 0].concat(objects));
      }
    ;
      return self;
    };

    def.$inspect = function() {
      var self = this;

      
      var result = [],
          id     = self.$__id__();

      for (var i = 0, length = self.length; i < length; i++) {
        var item = self['$[]'](i);

        if ((item).$__id__() === id) {
          result.push('[...]');
        }
        else {
          result.push((item).$inspect());
        }
      }

      return '[' + result.join(', ') + ']';
    ;
    };

    def.$join = function(sep) {
      var $a, self = this;
      if ($gvars[","] == null) $gvars[","] = nil;

      if (sep == null) {
        sep = nil
      }
      if ((($a = self.length === 0) !== nil && (!$a.$$is_boolean || $a == true))) {
        return ""};
      if ((($a = sep === nil) !== nil && (!$a.$$is_boolean || $a == true))) {
        sep = $gvars[","]};
      
      var result = [];

      for (var i = 0, length = self.length; i < length; i++) {
        var item = self[i];

        if ($scope.get('Opal')['$respond_to?'](item, "to_str")) {
          var tmp = (item).$to_str();

          if (tmp !== nil) {
            result.push((tmp).$to_s());

            continue;
          }
        }

        if ($scope.get('Opal')['$respond_to?'](item, "to_ary")) {
          var tmp = (item).$to_ary();

          if (tmp !== nil) {
            result.push((tmp).$join(sep));

            continue;
          }
        }

        if ($scope.get('Opal')['$respond_to?'](item, "to_s")) {
          var tmp = (item).$to_s();

          if (tmp !== nil) {
            result.push(tmp);

            continue;
          }
        }

        self.$raise($scope.get('NoMethodError'), "" + ($scope.get('Opal').$inspect(item)) + " doesn't respond to #to_str, #to_ary or #to_s");
      }

      if (sep === nil) {
        return result.join('');
      }
      else {
        return result.join($scope.get('Opal')['$coerce_to!'](sep, $scope.get('String'), "to_str").$to_s());
      }
    ;
    };

    def.$keep_if = TMP_11 = function() {
      var self = this, $iter = TMP_11.$$p, block = $iter || nil;

      TMP_11.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("keep_if")
      };
      
      for (var i = 0, length = self.length, value; i < length; i++) {
        if ((value = block(self[i])) === $breaker) {
          return $breaker.$v;
        }

        if (value === false || value === nil) {
          self.splice(i, 1);

          length--;
          i--;
        }
      }
    
      return self;
    };

    def.$last = function(count) {
      var self = this;

      
      if (count == null) {
        return self.length === 0 ? nil : self[self.length - 1];
      }

      count = $scope.get('Opal').$coerce_to(count, $scope.get('Integer'), "to_int");

      if (count < 0) {
        self.$raise($scope.get('ArgumentError'), "negative array size");
      }

      if (count > self.length) {
        count = self.length;
      }

      return self.slice(self.length - count, self.length);
    
    };

    def.$length = function() {
      var self = this;

      return self.length;
    };

    Opal.defn(self, '$map', def.$collect);

    Opal.defn(self, '$map!', def['$collect!']);

    def.$pop = function(count) {
      var $a, self = this;

      if ((($a = count === undefined) !== nil && (!$a.$$is_boolean || $a == true))) {
        if ((($a = self.length === 0) !== nil && (!$a.$$is_boolean || $a == true))) {
          return nil};
        return self.pop();};
      count = $scope.get('Opal').$coerce_to(count, $scope.get('Integer'), "to_int");
      if ((($a = count < 0) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('ArgumentError'), "negative array size")};
      if ((($a = self.length === 0) !== nil && (!$a.$$is_boolean || $a == true))) {
        return []};
      if ((($a = count > self.length) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self.splice(0, self.length);
        } else {
        return self.splice(self.length - count, self.length);
      };
    };

    def.$push = function(objects) {
      var self = this;

      objects = $slice.call(arguments, 0);
      
      for (var i = 0, length = objects.length; i < length; i++) {
        self.push(objects[i]);
      }
    
      return self;
    };

    def.$rassoc = function(object) {
      var self = this;

      
      for (var i = 0, length = self.length, item; i < length; i++) {
        item = self[i];

        if (item.length && item[1] !== undefined) {
          if ((item[1])['$=='](object)) {
            return item;
          }
        }
      }

      return nil;
    
    };

    def.$reject = TMP_12 = function() {
      var self = this, $iter = TMP_12.$$p, block = $iter || nil;

      TMP_12.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("reject")
      };
      
      var result = [];

      for (var i = 0, length = self.length, value; i < length; i++) {
        if ((value = block(self[i])) === $breaker) {
          return $breaker.$v;
        }

        if (value === false || value === nil) {
          result.push(self[i]);
        }
      }
      return result;
    
    };

    def['$reject!'] = TMP_13 = function() {
      var $a, $b, self = this, $iter = TMP_13.$$p, block = $iter || nil, original = nil;

      TMP_13.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("reject!")
      };
      original = self.$length();
      ($a = ($b = self).$delete_if, $a.$$p = block.$to_proc(), $a).call($b);
      if (self.$length()['$=='](original)) {
        return nil
        } else {
        return self
      };
    };

    def.$replace = function(other) {
      var $a, self = this;

      if ((($a = $scope.get('Array')['$==='](other)) !== nil && (!$a.$$is_boolean || $a == true))) {
        other = other.$to_a()
        } else {
        other = $scope.get('Opal').$coerce_to(other, $scope.get('Array'), "to_ary").$to_a()
      };
      
      self.splice(0, self.length);
      self.push.apply(self, other);
    
      return self;
    };

    def.$reverse = function() {
      var self = this;

      return self.slice(0).reverse();
    };

    def['$reverse!'] = function() {
      var self = this;

      return self.reverse();
    };

    def.$reverse_each = TMP_14 = function() {
      var $a, $b, self = this, $iter = TMP_14.$$p, block = $iter || nil;

      TMP_14.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("reverse_each")
      };
      ($a = ($b = self.$reverse()).$each, $a.$$p = block.$to_proc(), $a).call($b);
      return self;
    };

    def.$rindex = TMP_15 = function(object) {
      var self = this, $iter = TMP_15.$$p, block = $iter || nil;

      TMP_15.$$p = null;
      
      if (object != null) {
        for (var i = self.length - 1; i >= 0; i--) {
          if ((self[i])['$=='](object)) {
            return i;
          }
        }
      }
      else if (block !== nil) {
        for (var i = self.length - 1, value; i >= 0; i--) {
          if ((value = block(self[i])) === $breaker) {
            return $breaker.$v;
          }

          if (value !== false && value !== nil) {
            return i;
          }
        }
      }
      else if (object == null) {
        return self.$enum_for("rindex");
      }

      return nil;
    
    };

    def.$sample = function(n) {
      var $a, $b, TMP_16, self = this;

      if (n == null) {
        n = nil
      }
      if ((($a = ($b = n['$!'](), $b !== false && $b !== nil ?self['$empty?']() : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return nil};
      if ((($a = (($b = n !== false && n !== nil) ? self['$empty?']() : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return []};
      if (n !== false && n !== nil) {
        return ($a = ($b = ($range(1, n, false))).$map, $a.$$p = (TMP_16 = function(){var self = TMP_16.$$s || this;

        return self['$[]'](self.$rand(self.$length()))}, TMP_16.$$s = self, TMP_16), $a).call($b)
        } else {
        return self['$[]'](self.$rand(self.$length()))
      };
    };

    def.$select = TMP_17 = function() {
      var self = this, $iter = TMP_17.$$p, block = $iter || nil;

      TMP_17.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("select")
      };
      
      var result = [];

      for (var i = 0, length = self.length, item, value; i < length; i++) {
        item = self[i];

        if ((value = Opal.yield1(block, item)) === $breaker) {
          return $breaker.$v;
        }

        if (value !== false && value !== nil) {
          result.push(item);
        }
      }

      return result;
    
    };

    def['$select!'] = TMP_18 = function() {
      var $a, $b, self = this, $iter = TMP_18.$$p, block = $iter || nil;

      TMP_18.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("select!")
      };
      
      var original = self.length;
      ($a = ($b = self).$keep_if, $a.$$p = block.$to_proc(), $a).call($b);
      return self.length === original ? nil : self;
    
    };

    def.$shift = function(count) {
      var $a, self = this;

      if ((($a = count === undefined) !== nil && (!$a.$$is_boolean || $a == true))) {
        if ((($a = self.length === 0) !== nil && (!$a.$$is_boolean || $a == true))) {
          return nil};
        return self.shift();};
      count = $scope.get('Opal').$coerce_to(count, $scope.get('Integer'), "to_int");
      if ((($a = count < 0) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('ArgumentError'), "negative array size")};
      if ((($a = self.length === 0) !== nil && (!$a.$$is_boolean || $a == true))) {
        return []};
      return self.splice(0, count);
    };

    Opal.defn(self, '$size', def.$length);

    def.$shuffle = function() {
      var self = this;

      return self.$clone()['$shuffle!']();
    };

    def['$shuffle!'] = function() {
      var self = this;

      
      for (var i = self.length - 1; i > 0; i--) {
        var tmp = self[i],
            j   = Math.floor(Math.random() * (i + 1));

        self[i] = self[j];
        self[j] = tmp;
      }
    
      return self;
    };

    Opal.defn(self, '$slice', def['$[]']);

    def['$slice!'] = function(index, length) {
      var self = this;

      
      if (index < 0) {
        index += self.length;
      }

      if (length != null) {
        return self.splice(index, length);
      }

      if (index < 0 || index >= self.length) {
        return nil;
      }

      return self.splice(index, 1)[0];
    
    };

    def.$sort = TMP_19 = function() {
      var $a, self = this, $iter = TMP_19.$$p, block = $iter || nil;

      TMP_19.$$p = null;
      if ((($a = self.length > 1) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        return self
      };
      
      if (!(block !== nil)) {
        block = function(a, b) {
          return (a)['$<=>'](b);
        };
      }

      try {
        return self.slice().sort(function(x, y) {
          var ret = block(x, y);

          if (ret === $breaker) {
            throw $breaker;
          }
          else if (ret === nil) {
            self.$raise($scope.get('ArgumentError'), "comparison of " + ((x).$inspect()) + " with " + ((y).$inspect()) + " failed");
          }

          return (ret)['$>'](0) ? 1 : ((ret)['$<'](0) ? -1 : 0);
        });
      }
      catch (e) {
        if (e === $breaker) {
          return $breaker.$v;
        }
        else {
          throw e;
        }
      }
    ;
    };

    def['$sort!'] = TMP_20 = function() {
      var $a, $b, self = this, $iter = TMP_20.$$p, block = $iter || nil;

      TMP_20.$$p = null;
      
      var result;

      if ((block !== nil)) {
        result = ($a = ($b = (self.slice())).$sort, $a.$$p = block.$to_proc(), $a).call($b);
      }
      else {
        result = (self.slice()).$sort();
      }

      self.length = 0;
      for(var i = 0, length = result.length; i < length; i++) {
        self.push(result[i]);
      }

      return self;
    ;
    };

    def.$take = function(count) {
      var self = this;

      
      if (count < 0) {
        self.$raise($scope.get('ArgumentError'));
      }

      return self.slice(0, count);
    ;
    };

    def.$take_while = TMP_21 = function() {
      var self = this, $iter = TMP_21.$$p, block = $iter || nil;

      TMP_21.$$p = null;
      
      var result = [];

      for (var i = 0, length = self.length, item, value; i < length; i++) {
        item = self[i];

        if ((value = block(item)) === $breaker) {
          return $breaker.$v;
        }

        if (value === false || value === nil) {
          return result;
        }

        result.push(item);
      }

      return result;
    
    };

    def.$to_a = function() {
      var self = this;

      return self;
    };

    Opal.defn(self, '$to_ary', def.$to_a);

    Opal.defn(self, '$to_s', def.$inspect);

    def.$transpose = function() {
      var $a, $b, TMP_22, self = this, result = nil, max = nil;

      if ((($a = self['$empty?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
        return []};
      result = [];
      max = nil;
      ($a = ($b = self).$each, $a.$$p = (TMP_22 = function(row){var self = TMP_22.$$s || this, $a, $b, TMP_23;
if (row == null) row = nil;
      if ((($a = $scope.get('Array')['$==='](row)) !== nil && (!$a.$$is_boolean || $a == true))) {
          row = row.$to_a()
          } else {
          row = $scope.get('Opal').$coerce_to(row, $scope.get('Array'), "to_ary").$to_a()
        };
        ((($a = max) !== false && $a !== nil) ? $a : max = row.length);
        if ((($a = (row.length)['$=='](max)['$!']()) !== nil && (!$a.$$is_boolean || $a == true))) {
          self.$raise($scope.get('IndexError'), "element size differs (" + (row.length) + " should be " + (max))};
        return ($a = ($b = (row.length)).$times, $a.$$p = (TMP_23 = function(i){var self = TMP_23.$$s || this, $a, $b, $c, entry = nil;
if (i == null) i = nil;
        entry = (($a = i, $b = result, ((($c = $b['$[]']($a)) !== false && $c !== nil) ? $c : $b['$[]=']($a, []))));
          return entry['$<<'](row.$at(i));}, TMP_23.$$s = self, TMP_23), $a).call($b);}, TMP_22.$$s = self, TMP_22), $a).call($b);
      return result;
    };

    def.$uniq = function() {
      var self = this;

      
      var result = [],
          seen   = {};

      for (var i = 0, length = self.length, item, hash; i < length; i++) {
        item = self[i];
        hash = item;

        if (!seen[hash]) {
          seen[hash] = true;

          result.push(item);
        }
      }

      return result;
    
    };

    def['$uniq!'] = function() {
      var self = this;

      
      var original = self.length,
          seen     = {};

      for (var i = 0, length = original, item, hash; i < length; i++) {
        item = self[i];
        hash = item;

        if (!seen[hash]) {
          seen[hash] = true;
        }
        else {
          self.splice(i, 1);

          length--;
          i--;
        }
      }

      return self.length === original ? nil : self;
    
    };

    def.$unshift = function(objects) {
      var self = this;

      objects = $slice.call(arguments, 0);
      
      for (var i = objects.length - 1; i >= 0; i--) {
        self.unshift(objects[i]);
      }
    
      return self;
    };

    return (def.$zip = TMP_24 = function(others) {
      var self = this, $iter = TMP_24.$$p, block = $iter || nil;

      others = $slice.call(arguments, 0);
      TMP_24.$$p = null;
      
      var result = [], size = self.length, part, o;

      for (var i = 0; i < size; i++) {
        part = [self[i]];

        for (var j = 0, jj = others.length; j < jj; j++) {
          o = others[j][i];

          if (o == null) {
            o = nil;
          }

          part[j + 1] = o;
        }

        result[i] = part;
      }

      if (block !== nil) {
        for (var i = 0; i < size; i++) {
          block(result[i]);
        }

        return nil;
      }

      return result;
    
    }, nil) && 'zip';
  })(self, null);
};

/* Generated by Opal 0.7.2 */
Opal.modules["corelib/array/inheritance"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  Opal.add_stubs(['$new', '$allocate', '$initialize', '$to_proc', '$__send__', '$clone', '$respond_to?', '$==', '$eql?', '$inspect', '$*', '$class', '$slice', '$uniq', '$flatten']);
  (function($base, $super) {
    function $Array(){};
    var self = $Array = $klass($base, $super, 'Array', $Array);

    var def = self.$$proto, $scope = self.$$scope;

    return (Opal.defs(self, '$inherited', function(klass) {
      var self = this, replace = nil;

      replace = $scope.get('Class').$new((($scope.get('Array')).$$scope.get('Wrapper')));
      
      klass.$$proto         = replace.$$proto;
      klass.$$proto.$$class = klass;
      klass.$$alloc         = replace.$$alloc;
      klass.$$parent        = (($scope.get('Array')).$$scope.get('Wrapper'));

      klass.$allocate = replace.$allocate;
      klass.$new      = replace.$new;
      klass["$[]"]    = replace["$[]"];
    
    }), nil) && 'inherited'
  })(self, null);
  return (function($base, $super) {
    function $Wrapper(){};
    var self = $Wrapper = $klass($base, $super, 'Wrapper', $Wrapper);

    var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_2, TMP_3, TMP_4, TMP_5;

    def.literal = nil;
    Opal.defs(self, '$allocate', TMP_1 = function(array) {
      var self = this, $iter = TMP_1.$$p, $yield = $iter || nil, obj = nil;

      if (array == null) {
        array = []
      }
      TMP_1.$$p = null;
      obj = Opal.find_super_dispatcher(self, 'allocate', TMP_1, null, $Wrapper).apply(self, []);
      obj.literal = array;
      return obj;
    });

    Opal.defs(self, '$new', TMP_2 = function(args) {
      var $a, $b, self = this, $iter = TMP_2.$$p, block = $iter || nil, obj = nil;

      args = $slice.call(arguments, 0);
      TMP_2.$$p = null;
      obj = self.$allocate();
      ($a = ($b = obj).$initialize, $a.$$p = block.$to_proc(), $a).apply($b, [].concat(args));
      return obj;
    });

    Opal.defs(self, '$[]', function(objects) {
      var self = this;

      objects = $slice.call(arguments, 0);
      return self.$allocate(objects);
    });

    def.$initialize = TMP_3 = function(args) {
      var $a, $b, self = this, $iter = TMP_3.$$p, block = $iter || nil;

      args = $slice.call(arguments, 0);
      TMP_3.$$p = null;
      return self.literal = ($a = ($b = $scope.get('Array')).$new, $a.$$p = block.$to_proc(), $a).apply($b, [].concat(args));
    };

    def.$method_missing = TMP_4 = function(args) {
      var $a, $b, self = this, $iter = TMP_4.$$p, block = $iter || nil, result = nil;

      args = $slice.call(arguments, 0);
      TMP_4.$$p = null;
      result = ($a = ($b = self.literal).$__send__, $a.$$p = block.$to_proc(), $a).apply($b, [].concat(args));
      if ((($a = result === self.literal) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self
        } else {
        return result
      };
    };

    def.$initialize_copy = function(other) {
      var self = this;

      return self.literal = (other.literal).$clone();
    };

    def['$respond_to?'] = TMP_5 = function(name) {var $zuper = $slice.call(arguments, 0);
      var $a, self = this, $iter = TMP_5.$$p, $yield = $iter || nil;

      TMP_5.$$p = null;
      return ((($a = Opal.find_super_dispatcher(self, 'respond_to?', TMP_5, $iter).apply(self, $zuper)) !== false && $a !== nil) ? $a : self.literal['$respond_to?'](name));
    };

    def['$=='] = function(other) {
      var self = this;

      return self.literal['$=='](other);
    };

    def['$eql?'] = function(other) {
      var self = this;

      return self.literal['$eql?'](other);
    };

    def.$to_a = function() {
      var self = this;

      return self.literal;
    };

    def.$to_ary = function() {
      var self = this;

      return self;
    };

    def.$inspect = function() {
      var self = this;

      return self.literal.$inspect();
    };

    def['$*'] = function(other) {
      var self = this;

      
      var result = self.literal['$*'](other);

      if (result.$$is_array) {
        return self.$class().$allocate(result)
      }
      else {
        return result;
      }
    ;
    };

    def['$[]'] = function(index, length) {
      var self = this;

      
      var result = self.literal.$slice(index, length);

      if (result.$$is_array && (index.$$is_range || length !== undefined)) {
        return self.$class().$allocate(result)
      }
      else {
        return result;
      }
    ;
    };

    Opal.defn(self, '$slice', def['$[]']);

    def.$uniq = function() {
      var self = this;

      return self.$class().$allocate(self.literal.$uniq());
    };

    return (def.$flatten = function(level) {
      var self = this;

      return self.$class().$allocate(self.literal.$flatten(level));
    }, nil) && 'flatten';
  })($scope.get('Array'), null);
};

/* Generated by Opal 0.7.2 */
Opal.modules["corelib/hash"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  Opal.add_stubs(['$require', '$include', '$!', '$==', '$call', '$coerce_to!', '$lambda?', '$abs', '$arity', '$raise', '$enum_for', '$flatten', '$eql?', '$===', '$clone', '$merge!', '$to_proc', '$alias_method']);
  self.$require("corelib/enumerable");
  return (function($base, $super) {
    function $Hash(){};
    var self = $Hash = $klass($base, $super, 'Hash', $Hash);

    var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_2, TMP_3, TMP_4, TMP_5, TMP_6, TMP_7, TMP_8, TMP_9, TMP_10, TMP_11, TMP_12, TMP_13;

    def.proc = def.none = nil;
    self.$include($scope.get('Enumerable'));

    def.$$is_hash = true;

    Opal.defs(self, '$[]', function(objs) {
      var self = this;

      objs = $slice.call(arguments, 0);
      return Opal.hash.apply(null, objs);
    });

    Opal.defs(self, '$allocate', function() {
      var self = this;

      
      var hash = new self.$$alloc;

      hash.map  = {};
      hash.smap = {};
      hash.keys = [];
      hash.none = nil;
      hash.proc = nil;

      return hash;
    
    });

    def.$initialize = TMP_1 = function(defaults) {
      var self = this, $iter = TMP_1.$$p, block = $iter || nil;

      TMP_1.$$p = null;
      
      self.none = (defaults === undefined ? nil : defaults);
      self.proc = block;
    
      return self;
    };

    def['$=='] = function(other) {
      var self = this;

      
      if (self === other) {
        return true;
      }

      if (!other.keys || !other.smap || !other.map) {
        return false;
      }

      if (self.keys.length !== other.keys.length) {
        return false;
      }

      var _map  = self.map,
          smap  = self.smap,
          _map2 = other.map,
          smap2 = other.smap,
          map, map2, key, khash, value, value2;

      for (var i = 0, length = self.keys.length; i < length; i++) {
        key = self.keys[i];

        if (key.$$is_string) {
          khash = key;
          map   = smap;
          map2  = smap2;
        } else {
          khash = key.$hash();
          map   = _map;
          map2  = _map2;
        }

        value  = map[khash];
        if (value === undefined) console.log('==', key, self);
        value2 = map2[khash];

        if (value2 === undefined || ((value)['$=='](value2))['$!']()) {
          return false;
        }
      }

      return true;
    
    };

    def['$[]'] = function(key) {
      var self = this;

      
      var map, khash;

      if (key.$$is_string) {
        map = self.smap;
        khash = key;
      } else {
        map = self.map;
        khash = key.$hash();
      }

      if (map === undefined) { console.log(self, '[] --> key:', key, khash, map) }


      if (Opal.hasOwnProperty.call(map, khash)) {
        return map[khash];
      }

      var proc = self.proc;

      if (proc !== nil) {
        return (proc).$call(self, key);
      }

      return self.none;
    
    };

    def['$[]='] = function(key, value) {
      var self = this;

      
      var map, khash, value;

      if (key.$$is_string) {
        map = self.smap;
        khash = key;
      } else {
        map = self.map;
        khash = key.$hash();
      }

      if (!Opal.hasOwnProperty.call(map, khash)) {
        self.keys.push(key);
      }

      map[khash] = value;

      return value;
    
    };

    def.$assoc = function(object) {
      var self = this;

      
      var keys = self.keys,
          map, key, khash;

      for (var i = 0, length = keys.length; i < length; i++) {
        key = keys[i];

        if ((key)['$=='](object)) {
          if (key.$$is_string) {
            map = self.smap;
            khash = key;
          } else {
            map = self.map;
            khash = key.$hash();
          }

          return [key, map[khash]];
        }
      }

      return nil;
    
    };

    def.$clear = function() {
      var self = this;

      
      self.map = {};
      self.smap = {};
      self.keys = [];
      return self;
    
    };

    def.$clone = function() {
      var self = this;

      
      var _map  = {},
          smap  = {},
          _map2 = self.map,
          smap2 = self.smap,
          keys  = [],
          map, map2, key, khash, value;

      for (var i = 0, length = self.keys.length; i < length; i++) {
        key   = self.keys[i];

        if (key.$$is_string) {
          khash = key;
          map = smap;
          map2 = smap2;
        } else {
          khash = key.$hash();
          map = _map;
          map2 = _map2;
        }

        value = map2[khash];

        keys.push(key);
        map[khash] = value;
      }

      var clone = new self.$$class.$$alloc();

      clone.map  = _map;
      clone.smap = smap;
      clone.keys = keys;
      clone.none = self.none;
      clone.proc = self.proc;

      return clone;
    
    };

    def.$default = function(val) {
      var self = this;

      
      if (val !== undefined && self.proc !== nil) {
        return self.proc.$call(self, val);
      }
      return self.none;
    ;
    };

    def['$default='] = function(object) {
      var self = this;

      
      self.proc = nil;
      return (self.none = object);
    
    };

    def.$default_proc = function() {
      var self = this;

      return self.proc;
    };

    def['$default_proc='] = function(proc) {
      var self = this;

      
      if (proc !== nil) {
        proc = $scope.get('Opal')['$coerce_to!'](proc, $scope.get('Proc'), "to_proc");

        if (proc['$lambda?']() && proc.$arity().$abs() != 2) {
          self.$raise($scope.get('TypeError'), "default_proc takes two arguments");
        }
      }
      self.none = nil;
      return (self.proc = proc);
    ;
    };

    def.$delete = TMP_2 = function(key) {
      var self = this, $iter = TMP_2.$$p, block = $iter || nil;

      TMP_2.$$p = null;
      
      var result, map, khash;

      if (key.$$is_string) {
        map = self.smap;
        khash = key;
      } else {
        map = self.map;
        khash = key.$hash();
      }

      result = map[khash];

      if (result != null) {
        delete map[khash];
        self.keys.$delete(key);

        return result;
      }

      if (block !== nil) {
        return block.$call(key);
      }
      return nil;
    
    };

    def.$delete_if = TMP_3 = function() {
      var self = this, $iter = TMP_3.$$p, block = $iter || nil;

      TMP_3.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        return self.$enum_for("delete_if")
      };
      
      var _map = self.map,
          smap = self.smap,
          keys = self.keys,
          map, key, value, obj, khash;

      for (var i = 0, length = keys.length; i < length; i++) {
        key = keys[i];

        if (key.$$is_string) {
          map = smap;
          khash = key;
        } else {
          map = _map;
          khash = key.$hash();
        }
        obj = map[khash];
        value = block(key, obj);

        if (value === $breaker) {
          return $breaker.$v;
        }

        if (value !== false && value !== nil) {
          keys.splice(i, 1);
          delete map[khash];

          length--;
          i--;
        }
      }

      return self;
    
    };

    Opal.defn(self, '$dup', def.$clone);

    def.$each = TMP_4 = function() {
      var self = this, $iter = TMP_4.$$p, block = $iter || nil;

      TMP_4.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        return self.$enum_for("each")
      };
      
      var _map = self.map,
          smap = self.smap,
          keys = self.keys,
          map, key, khash, value;

      for (var i = 0, length = keys.length; i < length; i++) {
        key = keys[i];

        if (key.$$is_string) {
          map = smap;
          khash = key;
        } else {
          map = _map;
          khash = key.$hash();
        }

        value = Opal.yield1(block, [key, map[khash]]);

        if (value === $breaker) {
          return $breaker.$v;
        }
      }

      return self;
    
    };

    def.$each_key = TMP_5 = function() {
      var self = this, $iter = TMP_5.$$p, block = $iter || nil;

      TMP_5.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        return self.$enum_for("each_key")
      };
      
      var keys = self.keys, key;

      for (var i = 0, length = keys.length; i < length; i++) {
        key = keys[i];

        if (block(key) === $breaker) {
          return $breaker.$v;
        }
      }

      return self;
    
    };

    Opal.defn(self, '$each_pair', def.$each);

    def.$each_value = TMP_6 = function() {
      var self = this, $iter = TMP_6.$$p, block = $iter || nil;

      TMP_6.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        return self.$enum_for("each_value")
      };
      
      var _map = self.map,
          smap = self.smap,
          keys = self.keys;

      for (var i = 0, length = keys.length; i < length; i++) {
        key = keys[i];

        if (key.$$is_string) {
          map = smap;
          khash = key;
        } else {
          map = _map;
          khash = key.$hash();
        }

        if (block(map[khash]) === $breaker) {
          return $breaker.$v;
        }
      }

      return self;
    
    };

    def['$empty?'] = function() {
      var self = this;

      return self.keys.length === 0;
    };

    Opal.defn(self, '$eql?', def['$==']);

    def.$fetch = TMP_7 = function(key, defaults) {
      var self = this, $iter = TMP_7.$$p, block = $iter || nil;

      TMP_7.$$p = null;
      
      var map, khash, value;

      if (key.$$is_string) {
        khash = key;
        map = self.smap;
      } else {
        khash = key.$hash();
        map = self.map;
      }

      value = map[khash];

      if (value != null) {
        return value;
      }

      if (block !== nil) {
        var value;

        if ((value = block(key)) === $breaker) {
          return $breaker.$v;
        }

        return value;
      }

      if (defaults != null) {
        return defaults;
      }

      self.$raise($scope.get('KeyError'), "key not found");
    
    };

    def.$flatten = function(level) {
      var self = this;

      
      var _map = self.map,
          smap = self.smap,
          keys = self.keys,
          result = [],
          map, key, khash, value;

      for (var i = 0, length = keys.length; i < length; i++) {
        key = keys[i];

        if (key.$$is_string) {
          khash = key;
          map = smap;
        } else {
          khash = key.$hash();
          map = _map;
        }

        value = map[khash];

        result.push(key);

        if (value.$$is_array) {
          if (level == null || level === 1) {
            result.push(value);
          }
          else {
            result = result.concat((value).$flatten(level - 1));
          }
        }
        else {
          result.push(value);
        }
      }

      return result;
    
    };

    def['$has_key?'] = function(key) {
      var self = this;

      
      var keys = self.keys,
          map, khash;

      if (key.$$is_string) {
        khash = key;
        map = self.smap;
      } else {
        khash = key.$hash();
        map = self.map;
      }

      if (Opal.hasOwnProperty.call(map, khash)) {
        for (var i = 0, length = keys.length; i < length; i++) {
          if (!(key['$eql?'](keys[i]))['$!']()) {
            return true;
          }
        }
      }

      return false;
    
    };

    def['$has_value?'] = function(value) {
      var self = this;

      
      for (var khash in self.map) {
        if ((self.map[khash])['$=='](value)) {
          return true;
        }
      }

      return false;
    ;
    };

    var hash_ids = null;

    def.$hash = function() {
      var self = this;

      
      var top = (hash_ids === null);
      try {
        var key, value,
            hash = ['Hash'],
            keys = self.keys,
            id = self.$object_id(),
            counter = 0;

        if (top) {
          hash_ids = {}
        }

        if (hash_ids.hasOwnProperty(id)) {
          return 'self';
        }

        hash_ids[id] = true;

        for (var i = 0, length = keys.length; i < length; i++) {
          key   = keys[i];
          value = key.$$is_string ? self.smap[key] : self.map[key.$hash()];
          key   = key.$hash();
          value = (typeof(value) === 'undefined') ? '' : value.$hash();
          hash.push([key,value]);
        }

        return hash.sort().join();
      } finally {
        if (top) {
          hash_ids = null;
        }
      }
    
    };

    Opal.defn(self, '$include?', def['$has_key?']);

    def.$index = function(object) {
      var self = this;

      
      var _map = self.map,
          smap = self.smap,
          keys = self.keys,
          map, khash, key;

      for (var i = 0, length = keys.length; i < length; i++) {
        key = keys[i];

        if (key.$$is_string) {
          map = smap;
          khash = key;
        } else {
          map = _map;
          khash = key.$hash();
        }

        if ((map[khash])['$=='](object)) {
          return key;
        }
      }

      return nil;
    
    };

    def.$indexes = function(keys) {
      var self = this;

      keys = $slice.call(arguments, 0);
      
      var result = [],
          _map = self.map,
          smap = self.smap,
          map, key, khash, value;

      for (var i = 0, length = keys.length; i < length; i++) {
        key = keys[i];

        if (key.$$is_string) {
          khash = key;
          map = smap;
        } else {
          khash = key.$hash();
          map = _map;
        }

        value = map[khash];

        if (value != null) {
          result.push(value);
        }
        else {
          result.push(self.none);
        }
      }

      return result;
    
    };

    Opal.defn(self, '$indices', def.$indexes);

    var inspect_ids = null;

    def.$inspect = function() {
      var self = this;

      
      var top = (inspect_ids === null);
      try {

        var key, value,
            inspect = [],
            keys = self.keys
            id = self.$object_id(),
            counter = 0;

        if (top) {
          inspect_ids = {}
        }

        if (inspect_ids.hasOwnProperty(id)) {
          return '{...}';
        }

        inspect_ids[id] = true;

        for (var i = 0, length = keys.length; i < length; i++) {
          key   = keys[i];
          value = key.$$is_string ? self.smap[key] : self.map[key.$hash()];
          key   = key.$inspect();
          value = value.$inspect();
          inspect.push(key + '=>' + value);
        }

        return '{' + inspect.join(', ') + '}';
      } finally {

        if (top) {
          inspect_ids = null;
        }
      }
    
    };

    def.$invert = function() {
      var self = this;

      
      var result = Opal.hash(),
          keys = self.keys,
          _map = self.map,
          smap = self.smap,
          keys2 = result.keys,
          _map2 = result.map,
          smap2 = result.smap,
          map, map2, key, khash, value;

      for (var i = 0, length = keys.length; i < length; i++) {
        key = keys[i];

        if (key.$$is_string) {
          map = smap;
          khash = key;
        } else {
          map = _map;
          khash = key.$hash();
        }

        value = map[khash];
        keys2.push(value);

        if (value.$$is_string) {
          map2 = smap2;
          khash = value;
        } else {
          map2 = _map2;
          khash = value.$hash();
        }

        map2[khash] = key;
      }

      return result;
    
    };

    def.$keep_if = TMP_8 = function() {
      var self = this, $iter = TMP_8.$$p, block = $iter || nil;

      TMP_8.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        return self.$enum_for("keep_if")
      };
      
      var _map = self.map,
          smap = self.smap,
          keys = self.keys,
          map, key, khash, value, keep;

      for (var i = 0, length = keys.length; i < length; i++) {
        key = keys[i];

        if (key.$$is_string) {
          khash = key;
          map = smap;
        } else {
          khash = key.$hash();
          map = _map;
        }

        value = map[khash];
        keep  = block(key, value);

        if (keep === $breaker) {
          return $breaker.$v;
        }

        if (keep === false || keep === nil) {
          keys.splice(i, 1);
          delete map[khash];

          length--;
          i--;
        }
      }

      return self;
    
    };

    Opal.defn(self, '$key', def.$index);

    Opal.defn(self, '$key?', def['$has_key?']);

    def.$keys = function() {
      var self = this;

      return self.keys.slice(0);
    };

    def.$length = function() {
      var self = this;

      return self.keys.length;
    };

    Opal.defn(self, '$member?', def['$has_key?']);

    def.$merge = TMP_9 = function(other) {
      var $a, $b, self = this, $iter = TMP_9.$$p, block = $iter || nil, cloned = nil;

      TMP_9.$$p = null;
      if ((($a = $scope.get('Hash')['$==='](other)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        other = $scope.get('Opal')['$coerce_to!'](other, $scope.get('Hash'), "to_hash")
      };
      cloned = self.$clone();
      ($a = ($b = cloned)['$merge!'], $a.$$p = block.$to_proc(), $a).call($b, other);
      return cloned;
    };

    def['$merge!'] = TMP_10 = function(other) {
      var self = this, $iter = TMP_10.$$p, block = $iter || nil;

      TMP_10.$$p = null;
      
      if (! $scope.get('Hash')['$==='](other)) {
        other = $scope.get('Opal')['$coerce_to!'](other, $scope.get('Hash'), "to_hash");
      }

      var keys  = self.keys,
          _map  = self.map,
          smap  = self.smap,
          keys2 = other.keys,
          _map2 = other.map,
          smap2 = other.smap,
          map, map2, key, khash, value, value2;

      if (block === nil) {
        for (var i = 0, length = keys2.length; i < length; i++) {
          key = keys2[i];

          if (key.$$is_string) {
            khash = key;
            map = smap;
            map2 = smap2;
          } else {
            khash = key.$hash();
            map = _map;
            map2 = _map2;
          }

          if (map[khash] == null) {
            keys.push(key);
          }

          map[khash] = map2[khash];
        }
      }
      else {
        for (var i = 0, length = keys2.length; i < length; i++) {
          key    = keys2[i];

          if (key.$$is_string) {
            khash = key;
            map = smap;
            map2 = smap2;
          } else {
            khash = key.$hash();
            map = _map;
            map2 = _map2;
          }

          value  = map[khash];
          value2 = map2[khash];

          if (value == null) {
            keys.push(key);
            map[khash] = value2;
          }
          else {
            map[khash] = block(key, value, value2);
          }
        }
      }

      return self;
    ;
    };

    def.$rassoc = function(object) {
      var self = this;

      
      var keys = self.keys,
          _map = self.map,
          smap = self.smap,
          key, khash, value;

      for (var i = 0, length = keys.length; i < length; i++) {
        key = keys[i]

        if (key.$$is_string) {
          khash = key;
          map = smap;
        } else {
          khash = key.$hash();
          map = _map;
        }

        value = map[khash];

        if ((value)['$=='](object)) {
          return [key, value];
        }
      }

      return nil;
    
    };

    def.$reject = TMP_11 = function() {
      var self = this, $iter = TMP_11.$$p, block = $iter || nil;

      TMP_11.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        return self.$enum_for("reject")
      };
      
      var keys   = self.keys,
          _map    = self.map,
          smap    = self.smap,
          result = Opal.hash(),
          _map2   = result.map,
          smap2   = result.smap,
          keys2  = result.keys,
          map, map2, key, khash, object, value;

      for (var i = 0, length = keys.length; i < length; i++) {
        key = keys[i];

        if (key.$$is_string) {
          khash = key;
          map = smap;
          map2 = smap2;
        } else {
          khash = key.$hash();
          map = _map;
          map2 = _map2;
        }

        object = map[khash];

        if ((value = block(key, object)) === $breaker) {
          return $breaker.$v;
        }

        if (value === false || value === nil) {
          keys2.push(key);
          map2[khash] = object;
        }
      }

      return result;
    
    };

    def.$replace = function(other) {
      var self = this;

      
      var keys  = self.keys = [],
          _map  = self.map  = {},
          smap  = self.smap = {},
          _map2 = other.map,
          smap2 = other.smap,
          key, khash, map, map2;

      for (var i = 0, length = other.keys.length; i < length; i++) {
        key = other.keys[i];

        if (key.$$is_string) {
          khash = key;
          map = smap;
          map2 = smap2;
        } else {
          khash = key.$hash();
          map = _map;
          map2 = _map2;
        }

        keys.push(key);
        map[khash] = map2[khash];
      }

      return self;
    
    };

    def.$select = TMP_12 = function() {
      var self = this, $iter = TMP_12.$$p, block = $iter || nil;

      TMP_12.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        return self.$enum_for("select")
      };
      
      var keys   = self.keys,
          _map   = self.map,
          smap   = self.smap,
          result = Opal.hash(),
          _map2  = result.map,
          smap2  = result.smap,
          keys2  = result.keys,
          map, map2, key, khash, value, object;

      for (var i = 0, length = keys.length; i < length; i++) {
        key = keys[i];

        if (key.$$is_string) {
          khash = key;
          map = smap;
          map2 = smap2;
        } else {
          khash = key.$hash();
          map = _map;
          map2 = _map2;
        }

        value = map[khash];
        object = block(key, value);

        if (object === $breaker) {
          return $breaker.$v;
        }

        if (object !== false && object !== nil) {
          keys2.push(key);
          map2[khash] = value;
        }
      }

      return result;
    
    };

    def['$select!'] = TMP_13 = function() {
      var self = this, $iter = TMP_13.$$p, block = $iter || nil;

      TMP_13.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        return self.$enum_for("select!")
      };
      
      var _map = self.map,
          smap = self.smap,
          keys = self.keys,
          result = nil,
          key, khash, value, object;

      for (var i = 0, length = keys.length; i < length; i++) {
        key = keys[i];

        if (key.$$is_string) {
          khash = key;
          map = smap;
        } else {
          khash = key.$hash();
          map = _map;
        }

        value = map[khash];
        object = block(key, value);

        if (object === $breaker) {
          return $breaker.$v;
        }

        if (object === false || object === nil) {
          keys.splice(i, 1);
          delete map[khash];

          length--;
          i--;
          result = self
        }
      }

      return result;
    
    };

    def.$shift = function() {
      var self = this;

      
      var keys = self.keys,
          _map = self.map,
          smap = self.smap,
          map, key, khash, value;

      if (keys.length) {
        key = keys[0];
        if (key.$$is_string) {
          khash = key;
          map = smap;
        } else {
          khash = key.$hash();
          map = _map;
        }
        value = map[khash];

        delete map[khash];
        keys.splice(0, 1);

        return [key, value];
      }

      return nil;
    
    };

    Opal.defn(self, '$size', def.$length);

    self.$alias_method("store", "[]=");

    def.$to_a = function() {
      var self = this;

      
      var keys = self.keys,
          _map = self.map,
          smap = self.smap,
          result = [],
          map, key;

      for (var i = 0, length = keys.length; i < length; i++) {
        key = keys[i];

        if (key.$$is_string) {
          khash = key;
          map = smap;
        } else {
          khash = key.$hash();
          map = _map;
        }

        result.push([key, map[khash]]);
      }

      return result;
    
    };

    def.$to_h = function() {
      var self = this;

      
      if (self.$$class === Opal.Hash) {
        return self
      }

      var hash   = new Opal.Hash.$$alloc,
          cloned = self.$clone();

      hash.map  = cloned.map;
      hash.smap = cloned.smap;
      hash.keys = cloned.keys;
      hash.none = cloned.none;
      hash.proc = cloned.proc;

      return hash;
    ;
    };

    def.$to_hash = function() {
      var self = this;

      return self;
    };

    Opal.defn(self, '$to_s', def.$inspect);

    Opal.defn(self, '$update', def['$merge!']);

    Opal.defn(self, '$value?', def['$has_value?']);

    Opal.defn(self, '$values_at', def.$indexes);

    return (def.$values = function() {
      var self = this;

      
      var _map = self.map,
          smap = self.smap,
          keys = self.keys,
          result = [],
          map, khash;

      for (var i = 0, length = keys.length; i < length; i++) {
        key = keys[i];

        if (key.$$is_string) {
          khash = key;
          map = smap;
        } else {
          khash = key.$hash();
          map = _map;
        }

        result.push(map[khash]);
      }

      return result;
    
    }, nil) && 'values';
  })(self, null);
};

/* Generated by Opal 0.7.2 */
Opal.modules["corelib/string"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass, $gvars = Opal.gvars;

  Opal.add_stubs(['$require', '$include', '$to_str', '$===', '$format', '$coerce_to', '$to_s', '$respond_to?', '$<=>', '$raise', '$=~', '$empty?', '$ljust', '$ceil', '$/', '$+', '$rjust', '$floor', '$to_a', '$each_char', '$to_proc', '$coerce_to!', '$initialize_clone', '$initialize_dup', '$enum_for', '$split', '$chomp', '$escape', '$class', '$to_i', '$!', '$each_line', '$match', '$new', '$try_convert', '$chars', '$&', '$join', '$is_a?', '$[]', '$str', '$value', '$proc', '$shift', '$__send__']);
  self.$require("corelib/comparable");
  (function($base, $super) {
    function $String(){};
    var self = $String = $klass($base, $super, 'String', $String);

    var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_2, TMP_3, TMP_4, TMP_5, TMP_6, TMP_7;

    def.length = nil;
    self.$include($scope.get('Comparable'));

    def.$$is_string = true;

    Opal.defs(self, '$try_convert', function(what) {
      var self = this;

      try {
      return what.$to_str()
      } catch ($err) {if (true) {
        return nil
        }else { throw $err; }
      };
    });

    Opal.defs(self, '$new', function(str) {
      var self = this;

      if (str == null) {
        str = ""
      }
      return new String(str);
    });

    def['$%'] = function(data) {
      var $a, self = this;

      if ((($a = $scope.get('Array')['$==='](data)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return ($a = self).$format.apply($a, [self].concat(data))
        } else {
        return self.$format(self, data)
      };
    };

    def['$*'] = function(count) {
      var self = this;

      
      if (count < 1) {
        return '';
      }

      var result  = '',
          pattern = self;

      while (count > 0) {
        if (count & 1) {
          result += pattern;
        }

        count >>= 1;
        pattern += pattern;
      }

      return result;
    
    };

    def['$+'] = function(other) {
      var self = this;

      other = $scope.get('Opal').$coerce_to(other, $scope.get('String'), "to_str");
      return self + other.$to_s();
    };

    def['$<=>'] = function(other) {
      var $a, self = this;

      if ((($a = other['$respond_to?']("to_str")) !== nil && (!$a.$$is_boolean || $a == true))) {
        other = other.$to_str().$to_s();
        return self > other ? 1 : (self < other ? -1 : 0);
        } else {
        
        var cmp = other['$<=>'](self);

        if (cmp === nil) {
          return nil;
        }
        else {
          return cmp > 0 ? -1 : (cmp < 0 ? 1 : 0);
        }
      ;
      };
    };

    def['$<<'] = function(other) {
      var self = this;

      return self.$raise($scope.get('NotImplementedError'), "#<< not supported. Mutable String methods are not supported in Opal.");
    };

    def['$=='] = function(other) {
      var $a, self = this;

      if ((($a = $scope.get('String')['$==='](other)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        return false
      };
      return self.$to_s() == other.$to_s();
    };

    Opal.defn(self, '$eql?', def['$==']);

    Opal.defn(self, '$===', def['$==']);

    def['$=~'] = function(other) {
      var self = this;

      
      if (other.$$is_string) {
        self.$raise($scope.get('TypeError'), "type mismatch: String given");
      }

      return other['$=~'](self);
    ;
    };

    def['$[]'] = function(index, length) {
      var self = this;

      
      var size = self.length;

      if (index.$$is_range) {
        var exclude = index.exclude,
            length  = index.end,
            index   = index.begin;

        if (index < 0) {
          index += size;
        }

        if (length < 0) {
          length += size;
        }

        if (!exclude) {
          length += 1;
        }

        if (index > size) {
          return nil;
        }

        length = length - index;

        if (length < 0) {
          length = 0;
        }

        return self.substr(index, length);
      }

      if (index < 0) {
        index += self.length;
      }

      if (length == null) {
        if (index >= self.length || index < 0) {
          return nil;
        }

        return self.substr(index, 1);
      }

      if (index > self.length || index < 0) {
        return nil;
      }

      return self.substr(index, length);
    
    };

    def.$capitalize = function() {
      var self = this;

      return self.charAt(0).toUpperCase() + self.substr(1).toLowerCase();
    };

    Opal.defn(self, '$capitalize!', def['$<<']);

    def.$casecmp = function(other) {
      var self = this;

      other = $scope.get('Opal').$coerce_to(other, $scope.get('String'), "to_str").$to_s();
      return (self.toLowerCase())['$<=>'](other.toLowerCase());
    };

    def.$center = function(width, padstr) {
      var $a, self = this;

      if (padstr == null) {
        padstr = " "
      }
      width = $scope.get('Opal').$coerce_to(width, $scope.get('Integer'), "to_int");
      padstr = $scope.get('Opal').$coerce_to(padstr, $scope.get('String'), "to_str").$to_s();
      if ((($a = padstr['$empty?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('ArgumentError'), "zero width padding")};
      if ((($a = width <= self.length) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self};
      
      var ljustified = self.$ljust((width['$+'](self.length))['$/'](2).$ceil(), padstr),
          rjustified = self.$rjust((width['$+'](self.length))['$/'](2).$floor(), padstr);

      return rjustified + ljustified.slice(self.length);
    ;
    };

    def.$chars = TMP_1 = function() {
      var $a, $b, self = this, $iter = TMP_1.$$p, block = $iter || nil;

      TMP_1.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        return self.$each_char().$to_a()
      };
      return ($a = ($b = self).$each_char, $a.$$p = block.$to_proc(), $a).call($b);
    };

    def.$chomp = function(separator) {
      var $a, self = this;
      if ($gvars["/"] == null) $gvars["/"] = nil;

      if (separator == null) {
        separator = $gvars["/"]
      }
      if ((($a = separator === nil || self.length === 0) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self};
      separator = $scope.get('Opal')['$coerce_to!'](separator, $scope.get('String'), "to_str").$to_s();
      
      if (separator === "\n") {
        return self.replace(/\r?\n?$/, '');
      }
      else if (separator === "") {
        return self.replace(/(\r?\n)+$/, '');
      }
      else if (self.length > separator.length) {
        var tail = self.substr(self.length - separator.length, separator.length);

        if (tail === separator) {
          return self.substr(0, self.length - separator.length);
        }
      }
    
      return self;
    };

    Opal.defn(self, '$chomp!', def['$<<']);

    def.$chop = function() {
      var self = this;

      
      var length = self.length;

      if (length <= 1) {
        return "";
      }

      if (self.charAt(length - 1) === "\n" && self.charAt(length - 2) === "\r") {
        return self.substr(0, length - 2);
      }
      else {
        return self.substr(0, length - 1);
      }
    
    };

    Opal.defn(self, '$chop!', def['$<<']);

    def.$chr = function() {
      var self = this;

      return self.charAt(0);
    };

    def.$clone = function() {
      var self = this, copy = nil;

      copy = self.slice();
      copy.$initialize_clone(self);
      return copy;
    };

    def.$dup = function() {
      var self = this, copy = nil;

      copy = self.slice();
      copy.$initialize_dup(self);
      return copy;
    };

    def.$count = function(str) {
      var self = this;

      return (self.length - self.replace(new RegExp(str, 'g'), '').length) / str.length;
    };

    Opal.defn(self, '$dup', def.$clone);

    def.$downcase = function() {
      var self = this;

      return self.toLowerCase();
    };

    Opal.defn(self, '$downcase!', def['$<<']);

    def.$each_char = TMP_2 = function() {
      var $a, self = this, $iter = TMP_2.$$p, block = $iter || nil;

      TMP_2.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("each_char")
      };
      
      for (var i = 0, length = self.length; i < length; i++) {
        ((($a = Opal.yield1(block, self.charAt(i))) === $breaker) ? $breaker.$v : $a);
      }
    
      return self;
    };

    def.$each_line = TMP_3 = function(separator) {
      var $a, self = this, $iter = TMP_3.$$p, $yield = $iter || nil;
      if ($gvars["/"] == null) $gvars["/"] = nil;

      if (separator == null) {
        separator = $gvars["/"]
      }
      TMP_3.$$p = null;
      if (($yield !== nil)) {
        } else {
        return self.$split(separator)
      };
      
      var chomped  = self.$chomp(),
          trailing = self.length != chomped.length,
          splitted = chomped.split(separator);

      for (var i = 0, length = splitted.length; i < length; i++) {
        if (i < length - 1 || trailing) {
          ((($a = Opal.yield1($yield, splitted[i] + separator)) === $breaker) ? $breaker.$v : $a);
        }
        else {
          ((($a = Opal.yield1($yield, splitted[i])) === $breaker) ? $breaker.$v : $a);
        }
      }
    ;
      return self;
    };

    def['$empty?'] = function() {
      var self = this;

      return self.length === 0;
    };

    def['$end_with?'] = function(suffixes) {
      var self = this;

      suffixes = $slice.call(arguments, 0);
      
      for (var i = 0, length = suffixes.length; i < length; i++) {
        var suffix = $scope.get('Opal').$coerce_to(suffixes[i], $scope.get('String'), "to_str").$to_s();

        if (self.length >= suffix.length &&
            self.substr(self.length - suffix.length, suffix.length) == suffix) {
          return true;
        }
      }
    
      return false;
    };

    Opal.defn(self, '$eql?', def['$==']);

    Opal.defn(self, '$equal?', def['$===']);

    def.$gsub = TMP_4 = function(pattern, replace) {
      var $a, $b, self = this, $iter = TMP_4.$$p, block = $iter || nil;

      TMP_4.$$p = null;
      if ((($a = ((($b = $scope.get('String')['$==='](pattern)) !== false && $b !== nil) ? $b : pattern['$respond_to?']("to_str"))) !== nil && (!$a.$$is_boolean || $a == true))) {
        pattern = (new RegExp("" + $scope.get('Regexp').$escape(pattern.$to_str())))};
      if ((($a = $scope.get('Regexp')['$==='](pattern)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise($scope.get('TypeError'), "wrong argument type " + (pattern.$class()) + " (expected Regexp)")
      };
      
      var pattern = pattern.toString(),
          options = pattern.substr(pattern.lastIndexOf('/') + 1) + 'g',
          regexp  = pattern.substr(1, pattern.lastIndexOf('/') - 1);

      self.$sub.$$p = block;
      return self.$sub(new RegExp(regexp, options), replace);
    
    };

    Opal.defn(self, '$gsub!', def['$<<']);

    def.$hash = function() {
      var self = this;

      return self.toString();
    };

    def.$hex = function() {
      var self = this;

      return self.$to_i(16);
    };

    def['$include?'] = function(other) {
      var $a, self = this;

      
      if (other.$$is_string) {
        return self.indexOf(other) !== -1;
      }
    
      if ((($a = other['$respond_to?']("to_str")) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise($scope.get('TypeError'), "no implicit conversion of " + (other.$class()) + " into String")
      };
      return self.indexOf(other.$to_str()) !== -1;
    };

    def.$index = function(what, offset) {
      var $a, self = this, result = nil;

      if (offset == null) {
        offset = nil
      }
      if ((($a = $scope.get('String')['$==='](what)) !== nil && (!$a.$$is_boolean || $a == true))) {
        what = what.$to_s()
      } else if ((($a = what['$respond_to?']("to_str")) !== nil && (!$a.$$is_boolean || $a == true))) {
        what = what.$to_str().$to_s()
      } else if ((($a = $scope.get('Regexp')['$==='](what)['$!']()) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('TypeError'), "type mismatch: " + (what.$class()) + " given")};
      result = -1;
      if (offset !== false && offset !== nil) {
        offset = $scope.get('Opal').$coerce_to(offset, $scope.get('Integer'), "to_int");
        
        var size = self.length;

        if (offset < 0) {
          offset = offset + size;
        }

        if (offset > size) {
          return nil;
        }
      
        if ((($a = $scope.get('Regexp')['$==='](what)) !== nil && (!$a.$$is_boolean || $a == true))) {
          result = ((($a = (what['$=~'](self.substr(offset)))) !== false && $a !== nil) ? $a : -1)
          } else {
          result = self.substr(offset).indexOf(what)
        };
        
        if (result !== -1) {
          result += offset;
        }
      
      } else if ((($a = $scope.get('Regexp')['$==='](what)) !== nil && (!$a.$$is_boolean || $a == true))) {
        result = ((($a = (what['$=~'](self))) !== false && $a !== nil) ? $a : -1)
        } else {
        result = self.indexOf(what)
      };
      if ((($a = result === -1) !== nil && (!$a.$$is_boolean || $a == true))) {
        return nil
        } else {
        return result
      };
    };

    def.$inspect = function() {
      var self = this;

      
      var escapable = /[\\\"\x00-\x1f\x7f-\x9f\u00ad\u0600-\u0604\u070f\u17b4\u17b5\u200c-\u200f\u2028-\u202f\u2060-\u206f\ufeff\ufff0-\uffff]/g,
          meta      = {
            '\b': '\\b',
            '\t': '\\t',
            '\n': '\\n',
            '\f': '\\f',
            '\r': '\\r',
            '"' : '\\"',
            '\\': '\\\\'
          };

      escapable.lastIndex = 0;

      return escapable.test(self) ? '"' + self.replace(escapable, function(a) {
        var c = meta[a];

        return typeof c === 'string' ? c :
          '\\u' + ('0000' + a.charCodeAt(0).toString(16)).slice(-4);
      }) + '"' : '"' + self + '"';
    
    };

    def.$intern = function() {
      var self = this;

      return self;
    };

    def.$lines = function(separator) {
      var self = this;
      if ($gvars["/"] == null) $gvars["/"] = nil;

      if (separator == null) {
        separator = $gvars["/"]
      }
      return self.$each_line(separator).$to_a();
    };

    def.$length = function() {
      var self = this;

      return self.length;
    };

    def.$ljust = function(width, padstr) {
      var $a, self = this;

      if (padstr == null) {
        padstr = " "
      }
      width = $scope.get('Opal').$coerce_to(width, $scope.get('Integer'), "to_int");
      padstr = $scope.get('Opal').$coerce_to(padstr, $scope.get('String'), "to_str").$to_s();
      if ((($a = padstr['$empty?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('ArgumentError'), "zero width padding")};
      if ((($a = width <= self.length) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self};
      
      var index  = -1,
          result = "";

      width -= self.length;

      while (++index < width) {
        result += padstr;
      }

      return self + result.slice(0, width);
    
    };

    def.$lstrip = function() {
      var self = this;

      return self.replace(/^\s*/, '');
    };

    Opal.defn(self, '$lstrip!', def['$<<']);

    def.$match = TMP_5 = function(pattern, pos) {
      var $a, $b, self = this, $iter = TMP_5.$$p, block = $iter || nil;

      TMP_5.$$p = null;
      if ((($a = ((($b = $scope.get('String')['$==='](pattern)) !== false && $b !== nil) ? $b : pattern['$respond_to?']("to_str"))) !== nil && (!$a.$$is_boolean || $a == true))) {
        pattern = (new RegExp("" + $scope.get('Regexp').$escape(pattern.$to_str())))};
      if ((($a = $scope.get('Regexp')['$==='](pattern)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise($scope.get('TypeError'), "wrong argument type " + (pattern.$class()) + " (expected Regexp)")
      };
      return ($a = ($b = pattern).$match, $a.$$p = block.$to_proc(), $a).call($b, self, pos);
    };

    def.$next = function() {
      var self = this;

      
      if (self.length === 0) {
        return "";
      }

      var initial = self.substr(0, self.length - 1);
      var last    = String.fromCharCode(self.charCodeAt(self.length - 1) + 1);

      return initial + last;
    
    };

    Opal.defn(self, '$next!', def['$<<']);

    def.$ord = function() {
      var self = this;

      return self.charCodeAt(0);
    };

    def.$partition = function(str) {
      var self = this;

      
      var result = self.split(str);
      var splitter = (result[0].length === self.length ? "" : str);

      return [result[0], splitter, result.slice(1).join(str.toString())];
    
    };

    def.$reverse = function() {
      var self = this;

      return self.split('').reverse().join('');
    };

    Opal.defn(self, '$reverse!', def['$<<']);

    def.$rindex = function(search, offset) {
      var self = this;

      
      var search_type = (search == null ? Opal.NilClass : search.constructor);
      if (search_type != String && search_type != RegExp) {
        var msg = "type mismatch: " + search_type + " given";
        self.$raise($scope.get('TypeError').$new(msg));
      }

      if (self.length == 0) {
        return search.length == 0 ? 0 : nil;
      }

      var result = -1;
      if (offset != null) {
        if (offset < 0) {
          offset = self.length + offset;
        }

        if (search_type == String) {
          result = self.lastIndexOf(search, offset);
        }
        else {
          result = self.substr(0, offset + 1).$reverse().search(search);
          if (result !== -1) {
            result = offset - result;
          }
        }
      }
      else {
        if (search_type == String) {
          result = self.lastIndexOf(search);
        }
        else {
          result = self.$reverse().search(search);
          if (result !== -1) {
            result = self.length - 1 - result;
          }
        }
      }

      return result === -1 ? nil : result;
    
    };

    def.$rjust = function(width, padstr) {
      var $a, self = this;

      if (padstr == null) {
        padstr = " "
      }
      width = $scope.get('Opal').$coerce_to(width, $scope.get('Integer'), "to_int");
      padstr = $scope.get('Opal').$coerce_to(padstr, $scope.get('String'), "to_str").$to_s();
      if ((($a = padstr['$empty?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('ArgumentError'), "zero width padding")};
      if ((($a = width <= self.length) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self};
      
      var chars     = Math.floor(width - self.length),
          patterns  = Math.floor(chars / padstr.length),
          result    = Array(patterns + 1).join(padstr),
          remaining = chars - result.length;

      return result + padstr.slice(0, remaining) + self;
    
    };

    def.$rstrip = function() {
      var self = this;

      return self.replace(/\s*$/, '');
    };

    def.$scan = TMP_6 = function(pattern) {
      var self = this, $iter = TMP_6.$$p, block = $iter || nil;

      TMP_6.$$p = null;
      
      if (pattern.global) {
        // should we clear it afterwards too?
        pattern.lastIndex = 0;
      }
      else {
        // rewrite regular expression to add the global flag to capture pre/post match
        pattern = new RegExp(pattern.source, 'g' + (pattern.multiline ? 'm' : '') + (pattern.ignoreCase ? 'i' : ''));
      }

      var result = [];
      var match;

      while ((match = pattern.exec(self)) != null) {
        var match_data = $scope.get('MatchData').$new(pattern, match);
        if (block === nil) {
          match.length == 1 ? result.push(match[0]) : result.push(match.slice(1));
        }
        else {
          match.length == 1 ? block(match[0]) : block.apply(self, match.slice(1));
        }
      }

      return (block !== nil ? self : result);
    
    };

    Opal.defn(self, '$size', def.$length);

    Opal.defn(self, '$slice', def['$[]']);

    Opal.defn(self, '$slice!', def['$<<']);

    def.$split = function(pattern, limit) {
      var self = this, $a;
      if ($gvars[";"] == null) $gvars[";"] = nil;

      if (pattern == null) {
        pattern = ((($a = $gvars[";"]) !== false && $a !== nil) ? $a : " ")
      }
      
      if (pattern === nil || pattern === undefined) {
        pattern = $gvars[";"];
      }

      var result = [];
      if (limit !== undefined) {
        limit = $scope.get('Opal')['$coerce_to!'](limit, $scope.get('Integer'), "to_int");
      }

      if (self.length === 0) {
        return [];
      }

      if (limit === 1) {
        return [self];
      }

      if (pattern && pattern.$$is_regexp) {
        var pattern_str = pattern.toString();

        /* Opal and JS's repr of an empty RE. */
        var blank_pattern = (pattern_str.substr(0, 3) == '/^/') ||
                  (pattern_str.substr(0, 6) == '/(?:)/');

        /* This is our fast path */
        if (limit === undefined || limit === 0) {
          result = self.split(blank_pattern ? /(?:)/ : pattern);
        }
        else {
          /* RegExp.exec only has sane behavior with global flag */
          if (! pattern.global) {
            pattern = eval(pattern_str + 'g');
          }

          var match_data;
          var prev_index = 0;
          pattern.lastIndex = 0;

          while ((match_data = pattern.exec(self)) !== null) {
            var segment = self.slice(prev_index, match_data.index);
            result.push(segment);

            prev_index = pattern.lastIndex;

            if (match_data[0].length === 0) {
              if (blank_pattern) {
                /* explicitly split on JS's empty RE form.*/
                pattern = /(?:)/;
              }

              result = self.split(pattern);
              /* with "unlimited", ruby leaves a trail on blanks. */
              if (limit !== undefined && limit < 0 && blank_pattern) {
                result.push('');
              }

              prev_index = undefined;
              break;
            }

            if (limit !== undefined && limit > 1 && result.length + 1 == limit) {
              break;
            }
          }

          if (prev_index !== undefined) {
            result.push(self.slice(prev_index, self.length));
          }
        }
      }
      else {
        var splitted = 0, start = 0, lim = 0;

        if (pattern === nil || pattern === undefined) {
          pattern = ' '
        } else {
          pattern = $scope.get('Opal').$try_convert(pattern, $scope.get('String'), "to_str").$to_s();
        }

        var string = (pattern == ' ') ? self.replace(/[\r\n\t\v]\s+/g, ' ')
                                      : self;
        var cursor = -1;
        while ((cursor = string.indexOf(pattern, start)) > -1 && cursor < string.length) {
          if (splitted + 1 === limit) {
            break;
          }

          if (pattern == ' ' && cursor == start) {
            start = cursor + 1;
            continue;
          }

          result.push(string.substr(start, pattern.length ? cursor - start : 1));
          splitted++;

          start = cursor + (pattern.length ? pattern.length : 1);
        }

        if (string.length > 0 && (limit < 0 || string.length > start)) {
          if (string.length == start) {
            result.push('');
          }
          else {
            result.push(string.substr(start, string.length));
          }
        }
      }

      if (limit === undefined || limit === 0) {
        while (result[result.length-1] === '') {
          result.length = result.length - 1;
        }
      }

      if (limit > 0) {
        var tail = result.slice(limit - 1).join('');
        result.splice(limit - 1, result.length - 1, tail);
      }

      return result;
    ;
    };

    def.$squeeze = function(sets) {
      var self = this;

      sets = $slice.call(arguments, 0);
      
      if (sets.length === 0) {
        return self.replace(/(.)\1+/g, '$1');
      }
    
      
      var set = $scope.get('Opal').$coerce_to(sets[0], $scope.get('String'), "to_str").$chars();

      for (var i = 1, length = sets.length; i < length; i++) {
        set = (set)['$&']($scope.get('Opal').$coerce_to(sets[i], $scope.get('String'), "to_str").$chars());
      }

      if (set.length === 0) {
        return self;
      }

      return self.replace(new RegExp("([" + $scope.get('Regexp').$escape((set).$join()) + "])\\1+", "g"), "$1");
    ;
    };

    Opal.defn(self, '$squeeze!', def['$<<']);

    def['$start_with?'] = function(prefixes) {
      var self = this;

      prefixes = $slice.call(arguments, 0);
      
      for (var i = 0, length = prefixes.length; i < length; i++) {
        var prefix = $scope.get('Opal').$coerce_to(prefixes[i], $scope.get('String'), "to_str").$to_s();

        if (self.indexOf(prefix) === 0) {
          return true;
        }
      }

      return false;
    
    };

    def.$strip = function() {
      var self = this;

      return self.replace(/^\s*/, '').replace(/\s*$/, '');
    };

    Opal.defn(self, '$strip!', def['$<<']);

    
    // convert Ruby back reference to JavaScript back reference
    function convertReplace(replace) {
      return replace.replace(
        /(^|[^\\])\\(\d)/g, function(a, b, c) { return b + '$' + c }
      ).replace(
        /(^|[^\\])(\\\\)+\\\\(\d)/g, '$1$2\\$3'
      ).replace(
        /(^|[^\\])(?:(\\)\\)+([^\\]|$)/g, '$1$2$3'
      );
    }
  

    def.$sub = TMP_7 = function(pattern, replace) {
      var self = this, $iter = TMP_7.$$p, block = $iter || nil;

      TMP_7.$$p = null;
      
      if (typeof(pattern) !== 'string' && !pattern.$$is_regexp) {
        pattern = $scope.get('Opal')['$coerce_to!'](pattern, $scope.get('String'), "to_str");
      }

      if (replace !== undefined) {
        if (replace['$is_a?']($scope.get('Hash'))) {
          return self.replace(pattern, function(str) {
            var value = replace['$[]'](self.$str());

            return (value == null) ? nil : self.$value().$to_s();
          });
        }
        else {
          if (typeof(replace) !== 'string') {
            replace = $scope.get('Opal')['$coerce_to!'](replace, $scope.get('String'), "to_str");
          }

          replace = convertReplace(replace);
          return self.replace(pattern, replace);
        }

      }
      else if (block != null && block !== nil) {
        return self.replace(pattern, function() {
          // FIXME: this should be a formal MatchData object with all the goodies
          var match_data = []
          for (var i = 0, len = arguments.length; i < len; i++) {
            var arg = arguments[i];
            if (arg == undefined) {
              match_data.push(nil);
            }
            else {
              match_data.push(arg);
            }
          }

          var str = match_data.pop();
          var offset = match_data.pop();
          var match_len = match_data.length;

          // $1, $2, $3 not being parsed correctly in Ruby code
          for (var i = 1; i < match_len; i++) {
            Opal.gvars[String(i)] = match_data[i];
          }
          $gvars["&"] = match_data[0];
          $gvars["~"] = match_data;
          return block(match_data[0]);
        });
      }
      else {
        self.$raise($scope.get('ArgumentError'), "wrong number of arguments (1 for 2)")
      }
    ;
    };

    Opal.defn(self, '$sub!', def['$<<']);

    Opal.defn(self, '$succ', def.$next);

    Opal.defn(self, '$succ!', def['$<<']);

    def.$sum = function(n) {
      var self = this;

      if (n == null) {
        n = 16
      }
      
      var result = 0;

      for (var i = 0, length = self.length; i < length; i++) {
        result += (self.charCodeAt(i) % ((1 << n) - 1));
      }

      return result;
    
    };

    def.$swapcase = function() {
      var self = this;

      
      var str = self.replace(/([a-z]+)|([A-Z]+)/g, function($0,$1,$2) {
        return $1 ? $0.toUpperCase() : $0.toLowerCase();
      });

      if (self.constructor === String) {
        return str;
      }

      return self.$class().$new(str);
    
    };

    Opal.defn(self, '$swapcase!', def['$<<']);

    def.$to_f = function() {
      var self = this;

      
      if (self.charAt(0) === '_') {
        return 0;
      }

      var result = parseFloat(self.replace(/_/g, ''));

      if (isNaN(result) || result == Infinity || result == -Infinity) {
        return 0;
      }
      else {
        return result;
      }
    
    };

    def.$to_i = function(base) {
      var self = this;

      if (base == null) {
        base = 10
      }
      
      var result = parseInt(self, base);

      if (isNaN(result)) {
        return 0;
      }

      return result;
    
    };

    def.$to_proc = function() {
      var $a, $b, TMP_8, self = this, sym = nil;

      sym = self;
      return ($a = ($b = self).$proc, $a.$$p = (TMP_8 = function(args){var self = TMP_8.$$s || this, block, $a, $b, obj = nil;
args = $slice.call(arguments, 0);
        block = TMP_8.$$p || nil, TMP_8.$$p = null;
      if ((($a = args['$empty?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
          self.$raise($scope.get('ArgumentError'), "no receiver given")};
        obj = args.$shift();
        return ($a = ($b = obj).$__send__, $a.$$p = block.$to_proc(), $a).apply($b, [sym].concat(args));}, TMP_8.$$s = self, TMP_8), $a).call($b);
    };

    def.$to_s = function() {
      var self = this;

      return self.toString();
    };

    Opal.defn(self, '$to_str', def.$to_s);

    Opal.defn(self, '$to_sym', def.$intern);

    def.$tr = function(from, to) {
      var self = this;

      
      if (from.length == 0 || from === to) {
        return self;
      }

      var subs = {};
      var from_chars = from.split('');
      var from_length = from_chars.length;
      var to_chars = to.split('');
      var to_length = to_chars.length;

      var inverse = false;
      var global_sub = null;
      if (from_chars[0] === '^') {
        inverse = true;
        from_chars.shift();
        global_sub = to_chars[to_length - 1]
        from_length -= 1;
      }

      var from_chars_expanded = [];
      var last_from = null;
      var in_range = false;
      for (var i = 0; i < from_length; i++) {
        var ch = from_chars[i];
        if (last_from == null) {
          last_from = ch;
          from_chars_expanded.push(ch);
        }
        else if (ch === '-') {
          if (last_from === '-') {
            from_chars_expanded.push('-');
            from_chars_expanded.push('-');
          }
          else if (i == from_length - 1) {
            from_chars_expanded.push('-');
          }
          else {
            in_range = true;
          }
        }
        else if (in_range) {
          var start = last_from.charCodeAt(0) + 1;
          var end = ch.charCodeAt(0);
          for (var c = start; c < end; c++) {
            from_chars_expanded.push(String.fromCharCode(c));
          }
          from_chars_expanded.push(ch);
          in_range = null;
          last_from = null;
        }
        else {
          from_chars_expanded.push(ch);
        }
      }

      from_chars = from_chars_expanded;
      from_length = from_chars.length;

      if (inverse) {
        for (var i = 0; i < from_length; i++) {
          subs[from_chars[i]] = true;
        }
      }
      else {
        if (to_length > 0) {
          var to_chars_expanded = [];
          var last_to = null;
          var in_range = false;
          for (var i = 0; i < to_length; i++) {
            var ch = to_chars[i];
            if (last_from == null) {
              last_from = ch;
              to_chars_expanded.push(ch);
            }
            else if (ch === '-') {
              if (last_to === '-') {
                to_chars_expanded.push('-');
                to_chars_expanded.push('-');
              }
              else if (i == to_length - 1) {
                to_chars_expanded.push('-');
              }
              else {
                in_range = true;
              }
            }
            else if (in_range) {
              var start = last_from.charCodeAt(0) + 1;
              var end = ch.charCodeAt(0);
              for (var c = start; c < end; c++) {
                to_chars_expanded.push(String.fromCharCode(c));
              }
              to_chars_expanded.push(ch);
              in_range = null;
              last_from = null;
            }
            else {
              to_chars_expanded.push(ch);
            }
          }

          to_chars = to_chars_expanded;
          to_length = to_chars.length;
        }

        var length_diff = from_length - to_length;
        if (length_diff > 0) {
          var pad_char = (to_length > 0 ? to_chars[to_length - 1] : '');
          for (var i = 0; i < length_diff; i++) {
            to_chars.push(pad_char);
          }
        }

        for (var i = 0; i < from_length; i++) {
          subs[from_chars[i]] = to_chars[i];
        }
      }

      var new_str = ''
      for (var i = 0, length = self.length; i < length; i++) {
        var ch = self.charAt(i);
        var sub = subs[ch];
        if (inverse) {
          new_str += (sub == null ? global_sub : ch);
        }
        else {
          new_str += (sub != null ? sub : ch);
        }
      }
      return new_str;
    
    };

    Opal.defn(self, '$tr!', def['$<<']);

    def.$tr_s = function(from, to) {
      var self = this;

      
      if (from.length == 0) {
        return self;
      }

      var subs = {};
      var from_chars = from.split('');
      var from_length = from_chars.length;
      var to_chars = to.split('');
      var to_length = to_chars.length;

      var inverse = false;
      var global_sub = null;
      if (from_chars[0] === '^') {
        inverse = true;
        from_chars.shift();
        global_sub = to_chars[to_length - 1]
        from_length -= 1;
      }

      var from_chars_expanded = [];
      var last_from = null;
      var in_range = false;
      for (var i = 0; i < from_length; i++) {
        var ch = from_chars[i];
        if (last_from == null) {
          last_from = ch;
          from_chars_expanded.push(ch);
        }
        else if (ch === '-') {
          if (last_from === '-') {
            from_chars_expanded.push('-');
            from_chars_expanded.push('-');
          }
          else if (i == from_length - 1) {
            from_chars_expanded.push('-');
          }
          else {
            in_range = true;
          }
        }
        else if (in_range) {
          var start = last_from.charCodeAt(0) + 1;
          var end = ch.charCodeAt(0);
          for (var c = start; c < end; c++) {
            from_chars_expanded.push(String.fromCharCode(c));
          }
          from_chars_expanded.push(ch);
          in_range = null;
          last_from = null;
        }
        else {
          from_chars_expanded.push(ch);
        }
      }

      from_chars = from_chars_expanded;
      from_length = from_chars.length;

      if (inverse) {
        for (var i = 0; i < from_length; i++) {
          subs[from_chars[i]] = true;
        }
      }
      else {
        if (to_length > 0) {
          var to_chars_expanded = [];
          var last_to = null;
          var in_range = false;
          for (var i = 0; i < to_length; i++) {
            var ch = to_chars[i];
            if (last_from == null) {
              last_from = ch;
              to_chars_expanded.push(ch);
            }
            else if (ch === '-') {
              if (last_to === '-') {
                to_chars_expanded.push('-');
                to_chars_expanded.push('-');
              }
              else if (i == to_length - 1) {
                to_chars_expanded.push('-');
              }
              else {
                in_range = true;
              }
            }
            else if (in_range) {
              var start = last_from.charCodeAt(0) + 1;
              var end = ch.charCodeAt(0);
              for (var c = start; c < end; c++) {
                to_chars_expanded.push(String.fromCharCode(c));
              }
              to_chars_expanded.push(ch);
              in_range = null;
              last_from = null;
            }
            else {
              to_chars_expanded.push(ch);
            }
          }

          to_chars = to_chars_expanded;
          to_length = to_chars.length;
        }

        var length_diff = from_length - to_length;
        if (length_diff > 0) {
          var pad_char = (to_length > 0 ? to_chars[to_length - 1] : '');
          for (var i = 0; i < length_diff; i++) {
            to_chars.push(pad_char);
          }
        }

        for (var i = 0; i < from_length; i++) {
          subs[from_chars[i]] = to_chars[i];
        }
      }
      var new_str = ''
      var last_substitute = null
      for (var i = 0, length = self.length; i < length; i++) {
        var ch = self.charAt(i);
        var sub = subs[ch]
        if (inverse) {
          if (sub == null) {
            if (last_substitute == null) {
              new_str += global_sub;
              last_substitute = true;
            }
          }
          else {
            new_str += ch;
            last_substitute = null;
          }
        }
        else {
          if (sub != null) {
            if (last_substitute == null || last_substitute !== sub) {
              new_str += sub;
              last_substitute = sub;
            }
          }
          else {
            new_str += ch;
            last_substitute = null;
          }
        }
      }
      return new_str;
    
    };

    Opal.defn(self, '$tr_s!', def['$<<']);

    def.$upcase = function() {
      var self = this;

      return self.toUpperCase();
    };

    Opal.defn(self, '$upcase!', def['$<<']);

    def.$freeze = function() {
      var self = this;

      return self;
    };

    return (def['$frozen?'] = function() {
      var self = this;

      return true;
    }, nil) && 'frozen?';
  })(self, null);
  return Opal.cdecl($scope, 'Symbol', $scope.get('String'));
};

/* Generated by Opal 0.7.2 */
Opal.modules["corelib/string/inheritance"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  Opal.add_stubs(['$new', '$allocate', '$initialize', '$to_proc', '$__send__', '$class', '$clone', '$respond_to?', '$==', '$inspect']);
  (function($base, $super) {
    function $String(){};
    var self = $String = $klass($base, $super, 'String', $String);

    var def = self.$$proto, $scope = self.$$scope;

    return (Opal.defs(self, '$inherited', function(klass) {
      var self = this, replace = nil;

      replace = $scope.get('Class').$new((($scope.get('String')).$$scope.get('Wrapper')));
      
      klass.$$proto         = replace.$$proto;
      klass.$$proto.$$class = klass;
      klass.$$alloc         = replace.$$alloc;
      klass.$$parent        = (($scope.get('String')).$$scope.get('Wrapper'));

      klass.$allocate = replace.$allocate;
      klass.$new      = replace.$new;
    
    }), nil) && 'inherited'
  })(self, null);
  return (function($base, $super) {
    function $Wrapper(){};
    var self = $Wrapper = $klass($base, $super, 'Wrapper', $Wrapper);

    var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_2, TMP_3, TMP_4;

    def.literal = nil;
    Opal.defs(self, '$allocate', TMP_1 = function(string) {
      var self = this, $iter = TMP_1.$$p, $yield = $iter || nil, obj = nil;

      if (string == null) {
        string = ""
      }
      TMP_1.$$p = null;
      obj = Opal.find_super_dispatcher(self, 'allocate', TMP_1, null, $Wrapper).apply(self, []);
      obj.literal = string;
      return obj;
    });

    Opal.defs(self, '$new', TMP_2 = function(args) {
      var $a, $b, self = this, $iter = TMP_2.$$p, block = $iter || nil, obj = nil;

      args = $slice.call(arguments, 0);
      TMP_2.$$p = null;
      obj = self.$allocate();
      ($a = ($b = obj).$initialize, $a.$$p = block.$to_proc(), $a).apply($b, [].concat(args));
      return obj;
    });

    Opal.defs(self, '$[]', function(objects) {
      var self = this;

      objects = $slice.call(arguments, 0);
      return self.$allocate(objects);
    });

    def.$initialize = function(string) {
      var self = this;

      if (string == null) {
        string = ""
      }
      return self.literal = string;
    };

    def.$method_missing = TMP_3 = function(args) {
      var $a, $b, self = this, $iter = TMP_3.$$p, block = $iter || nil, result = nil;

      args = $slice.call(arguments, 0);
      TMP_3.$$p = null;
      result = ($a = ($b = self.literal).$__send__, $a.$$p = block.$to_proc(), $a).apply($b, [].concat(args));
      if ((($a = result.$$is_string != null) !== nil && (!$a.$$is_boolean || $a == true))) {
        if ((($a = result == self.literal) !== nil && (!$a.$$is_boolean || $a == true))) {
          return self
          } else {
          return self.$class().$allocate(result)
        }
        } else {
        return result
      };
    };

    def.$initialize_copy = function(other) {
      var self = this;

      return self.literal = (other.literal).$clone();
    };

    def['$respond_to?'] = TMP_4 = function(name) {var $zuper = $slice.call(arguments, 0);
      var $a, self = this, $iter = TMP_4.$$p, $yield = $iter || nil;

      TMP_4.$$p = null;
      return ((($a = Opal.find_super_dispatcher(self, 'respond_to?', TMP_4, $iter).apply(self, $zuper)) !== false && $a !== nil) ? $a : self.literal['$respond_to?'](name));
    };

    def['$=='] = function(other) {
      var self = this;

      return self.literal['$=='](other);
    };

    Opal.defn(self, '$eql?', def['$==']);

    Opal.defn(self, '$===', def['$==']);

    def.$to_s = function() {
      var self = this;

      return self.literal;
    };

    def.$to_str = function() {
      var self = this;

      return self;
    };

    return (def.$inspect = function() {
      var self = this;

      return self.literal.$inspect();
    }, nil) && 'inspect';
  })($scope.get('String'), null);
};

/* Generated by Opal 0.7.2 */
Opal.modules["corelib/match_data"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass, $gvars = Opal.gvars;

  Opal.add_stubs(['$attr_reader', '$[]', '$===', '$!', '$==', '$raise', '$inspect']);
  return (function($base, $super) {
    function $MatchData(){};
    var self = $MatchData = $klass($base, $super, 'MatchData', $MatchData);

    var def = self.$$proto, $scope = self.$$scope;

    def.string = def.matches = def.begin = nil;
    self.$attr_reader("post_match", "pre_match", "regexp", "string");

    def.$initialize = function(regexp, match_groups) {
      var self = this;

      $gvars["~"] = self;
      self.regexp = regexp;
      self.begin = match_groups.index;
      self.string = match_groups.input;
      self.pre_match = self.string.substr(0, regexp.lastIndex - match_groups[0].length);
      self.post_match = self.string.substr(regexp.lastIndex);
      self.matches = [];
      
      for (var i = 0, length = match_groups.length; i < length; i++) {
        var group = match_groups[i];

        if (group == null) {
          self.matches.push(nil);
        }
        else {
          self.matches.push(group);
        }
      }
    
    };

    def['$[]'] = function(args) {
      var $a, self = this;

      args = $slice.call(arguments, 0);
      return ($a = self.matches)['$[]'].apply($a, [].concat(args));
    };

    def['$=='] = function(other) {
      var $a, $b, $c, $d, self = this;

      if ((($a = $scope.get('MatchData')['$==='](other)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        return false
      };
      return ($a = ($b = ($c = ($d = self.string == other.string, $d !== false && $d !== nil ?self.regexp == other.regexp : $d), $c !== false && $c !== nil ?self.pre_match == other.pre_match : $c), $b !== false && $b !== nil ?self.post_match == other.post_match : $b), $a !== false && $a !== nil ?self.begin == other.begin : $a);
    };

    def.$begin = function(pos) {
      var $a, $b, self = this;

      if ((($a = ($b = pos['$=='](0)['$!'](), $b !== false && $b !== nil ?pos['$=='](1)['$!']() : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('ArgumentError'), "MatchData#begin only supports 0th element")};
      return self.begin;
    };

    def.$captures = function() {
      var self = this;

      return self.matches.slice(1);
    };

    def.$inspect = function() {
      var self = this;

      
      var str = "#<MatchData " + (self.matches[0]).$inspect();

      for (var i = 1, length = self.matches.length; i < length; i++) {
        str += " " + i + ":" + (self.matches[i]).$inspect();
      }

      return str + ">";
    ;
    };

    def.$length = function() {
      var self = this;

      return self.matches.length;
    };

    Opal.defn(self, '$size', def.$length);

    def.$to_a = function() {
      var self = this;

      return self.matches;
    };

    def.$to_s = function() {
      var self = this;

      return self.matches[0];
    };

    return (def.$values_at = function(indexes) {
      var self = this;

      indexes = $slice.call(arguments, 0);
      
      var values       = [],
          match_length = self.matches.length;

      for (var i = 0, length = indexes.length; i < length; i++) {
        var pos = indexes[i];

        if (pos >= 0) {
          values.push(self.matches[pos]);
        }
        else {
          pos += match_length;

          if (pos > 0) {
            values.push(self.matches[pos]);
          }
          else {
            values.push(nil);
          }
        }
      }

      return values;
    ;
    }, nil) && 'values_at';
  })(self, null)
};

/* Generated by Opal 0.7.2 */
Opal.modules["corelib/numeric"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  Opal.add_stubs(['$require', '$include', '$coerce', '$===', '$raise', '$class', '$__send__', '$send_coerced', '$coerce_to!', '$-@', '$**', '$-', '$respond_to?', '$==', '$enum_for', '$gcd', '$lcm', '$<', '$>', '$floor', '$/', '$%']);
  self.$require("corelib/comparable");
  (function($base, $super) {
    function $Numeric(){};
    var self = $Numeric = $klass($base, $super, 'Numeric', $Numeric);

    var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_2, TMP_3, TMP_4, TMP_5, TMP_6;

    self.$include($scope.get('Comparable'));

    def.$$is_number = true;

    def.$coerce = function(other, type) {
      var self = this, $case = nil;

      if (type == null) {
        type = "operation"
      }
      try {
      
      if (other.$$is_number) {
        return [self, other];
      }
      else {
        return other.$coerce(self);
      }
    
      } catch ($err) {if (true) {
        return (function() {$case = type;if ("operation"['$===']($case)) {return self.$raise($scope.get('TypeError'), "" + (other.$class()) + " can't be coerced into Numeric")}else if ("comparison"['$===']($case)) {return self.$raise($scope.get('ArgumentError'), "comparison of " + (self.$class()) + " with " + (other.$class()) + " failed")}else { return nil }})()
        }else { throw $err; }
      };
    };

    def.$send_coerced = function(method, other) {
      var $a, self = this, type = nil, $case = nil, a = nil, b = nil;

      type = (function() {$case = method;if ("+"['$===']($case) || "-"['$===']($case) || "*"['$===']($case) || "/"['$===']($case) || "%"['$===']($case) || "&"['$===']($case) || "|"['$===']($case) || "^"['$===']($case) || "**"['$===']($case)) {return "operation"}else if (">"['$===']($case) || ">="['$===']($case) || "<"['$===']($case) || "<="['$===']($case) || "<=>"['$===']($case)) {return "comparison"}else { return nil }})();
      $a = Opal.to_ary(self.$coerce(other, type)), a = ($a[0] == null ? nil : $a[0]), b = ($a[1] == null ? nil : $a[1]);
      return a.$__send__(method, b);
    };

    def['$+'] = function(other) {
      var self = this;

      
      if (other.$$is_number) {
        return self + other;
      }
      else {
        return self.$send_coerced("+", other);
      }
    
    };

    def['$-'] = function(other) {
      var self = this;

      
      if (other.$$is_number) {
        return self - other;
      }
      else {
        return self.$send_coerced("-", other);
      }
    
    };

    def['$*'] = function(other) {
      var self = this;

      
      if (other.$$is_number) {
        return self * other;
      }
      else {
        return self.$send_coerced("*", other);
      }
    
    };

    def['$/'] = function(other) {
      var self = this;

      
      if (other.$$is_number) {
        return self / other;
      }
      else {
        return self.$send_coerced("/", other);
      }
    
    };

    def['$%'] = function(other) {
      var self = this;

      
      if (other.$$is_number) {
        if (other < 0 || self < 0) {
          return (self % other + other) % other;
        }
        else {
          return self % other;
        }
      }
      else {
        return self.$send_coerced("%", other);
      }
    
    };

    def['$&'] = function(other) {
      var self = this;

      
      if (other.$$is_number) {
        return self & other;
      }
      else {
        return self.$send_coerced("&", other);
      }
    
    };

    def['$|'] = function(other) {
      var self = this;

      
      if (other.$$is_number) {
        return self | other;
      }
      else {
        return self.$send_coerced("|", other);
      }
    
    };

    def['$^'] = function(other) {
      var self = this;

      
      if (other.$$is_number) {
        return self ^ other;
      }
      else {
        return self.$send_coerced("^", other);
      }
    
    };

    def['$<'] = function(other) {
      var self = this;

      
      if (other.$$is_number) {
        return self < other;
      }
      else {
        return self.$send_coerced("<", other);
      }
    
    };

    def['$<='] = function(other) {
      var self = this;

      
      if (other.$$is_number) {
        return self <= other;
      }
      else {
        return self.$send_coerced("<=", other);
      }
    
    };

    def['$>'] = function(other) {
      var self = this;

      
      if (other.$$is_number) {
        return self > other;
      }
      else {
        return self.$send_coerced(">", other);
      }
    
    };

    def['$>='] = function(other) {
      var self = this;

      
      if (other.$$is_number) {
        return self >= other;
      }
      else {
        return self.$send_coerced(">=", other);
      }
    
    };

    def['$<=>'] = function(other) {
      var self = this;

      try {
      
      if (other.$$is_number) {
        return self > other ? 1 : (self < other ? -1 : 0);
      }
      else {
        return self.$send_coerced("<=>", other);
      }
    
      } catch ($err) {if (Opal.rescue($err, [$scope.get('ArgumentError')])) {
        return nil
        }else { throw $err; }
      };
    };

    def['$<<'] = function(count) {
      var self = this;

      count = $scope.get('Opal')['$coerce_to!'](count, $scope.get('Integer'), "to_int");
      return count > 0 ? self << count : self >> -count;
    };

    def['$>>'] = function(count) {
      var self = this;

      count = $scope.get('Opal')['$coerce_to!'](count, $scope.get('Integer'), "to_int");
      return count > 0 ? self >> count : self << -count;
    };

    def['$[]'] = function(bit) {
      var self = this, min = nil, max = nil;

      bit = $scope.get('Opal')['$coerce_to!'](bit, $scope.get('Integer'), "to_int");
      min = ((2)['$**'](30))['$-@']();
      max = ((2)['$**'](30))['$-'](1);
      return (bit < min || bit > max) ? 0 : (self >> bit) % 2;
    };

    def['$+@'] = function() {
      var self = this;

      return +self;
    };

    def['$-@'] = function() {
      var self = this;

      return -self;
    };

    def['$~'] = function() {
      var self = this;

      return ~self;
    };

    def['$**'] = function(other) {
      var self = this;

      
      if (other.$$is_number) {
        return Math.pow(self, other);
      }
      else {
        return self.$send_coerced("**", other);
      }
    
    };

    def['$=='] = function(other) {
      var self = this;

      
      if (other.$$is_number) {
        return self == Number(other);
      }
      else if (other['$respond_to?']("==")) {
        return other['$=='](self);
      }
      else {
        return false;
      }
    ;
    };

    def.$abs = function() {
      var self = this;

      return Math.abs(self);
    };

    def.$ceil = function() {
      var self = this;

      return Math.ceil(self);
    };

    def.$chr = function(encoding) {
      var self = this;

      return String.fromCharCode(self);
    };

    def.$conj = function() {
      var self = this;

      return self;
    };

    Opal.defn(self, '$conjugate', def.$conj);

    def.$downto = TMP_1 = function(finish) {
      var self = this, $iter = TMP_1.$$p, block = $iter || nil;

      TMP_1.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        return self.$enum_for("downto", finish)
      };
      
      for (var i = self; i >= finish; i--) {
        if (block(i) === $breaker) {
          return $breaker.$v;
        }
      }
    
      return self;
    };

    Opal.defn(self, '$eql?', def['$==']);

    Opal.defn(self, '$equal?', def['$==']);

    def['$even?'] = function() {
      var self = this;

      return self % 2 === 0;
    };

    def.$floor = function() {
      var self = this;

      return Math.floor(self);
    };

    def.$gcd = function(other) {
      var $a, self = this;

      if ((($a = $scope.get('Integer')['$==='](other)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise($scope.get('TypeError'), "not an integer")
      };
      
      var min = Math.abs(self),
          max = Math.abs(other);

      while (min > 0) {
        var tmp = min;

        min = max % min;
        max = tmp;
      }

      return max;
    
    };

    def.$gcdlcm = function(other) {
      var self = this;

      return [self.$gcd(), self.$lcm()];
    };

    def.$hash = function() {
      var self = this;

      return 'Numeric:'+self.toString();
    };

    def['$integer?'] = function() {
      var self = this;

      return self % 1 === 0;
    };

    def['$is_a?'] = TMP_2 = function(klass) {var $zuper = $slice.call(arguments, 0);
      var $a, $b, self = this, $iter = TMP_2.$$p, $yield = $iter || nil;

      TMP_2.$$p = null;
      if ((($a = (($b = klass['$==']($scope.get('Fixnum'))) ? $scope.get('Integer')['$==='](self) : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return true};
      if ((($a = (($b = klass['$==']($scope.get('Integer'))) ? $scope.get('Integer')['$==='](self) : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return true};
      if ((($a = (($b = klass['$==']($scope.get('Float'))) ? $scope.get('Float')['$==='](self) : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return true};
      return Opal.find_super_dispatcher(self, 'is_a?', TMP_2, $iter).apply(self, $zuper);
    };

    Opal.defn(self, '$kind_of?', def['$is_a?']);

    def['$instance_of?'] = TMP_3 = function(klass) {var $zuper = $slice.call(arguments, 0);
      var $a, $b, self = this, $iter = TMP_3.$$p, $yield = $iter || nil;

      TMP_3.$$p = null;
      if ((($a = (($b = klass['$==']($scope.get('Fixnum'))) ? $scope.get('Integer')['$==='](self) : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return true};
      if ((($a = (($b = klass['$==']($scope.get('Integer'))) ? $scope.get('Integer')['$==='](self) : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return true};
      if ((($a = (($b = klass['$==']($scope.get('Float'))) ? $scope.get('Float')['$==='](self) : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return true};
      return Opal.find_super_dispatcher(self, 'instance_of?', TMP_3, $iter).apply(self, $zuper);
    };

    def.$lcm = function(other) {
      var $a, self = this;

      if ((($a = $scope.get('Integer')['$==='](other)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise($scope.get('TypeError'), "not an integer")
      };
      
      if (self == 0 || other == 0) {
        return 0;
      }
      else {
        return Math.abs(self * other / self.$gcd(other));
      }
    
    };

    Opal.defn(self, '$magnitude', def.$abs);

    Opal.defn(self, '$modulo', def['$%']);

    def.$next = function() {
      var self = this;

      return self + 1;
    };

    def['$nonzero?'] = function() {
      var self = this;

      return self == 0 ? nil : self;
    };

    def['$odd?'] = function() {
      var self = this;

      return self % 2 !== 0;
    };

    def.$ord = function() {
      var self = this;

      return self;
    };

    def.$pred = function() {
      var self = this;

      return self - 1;
    };

    def.$round = function(ndigits) {
      var self = this;

      if (ndigits == null) {
        ndigits = 0
      }
      
      var scale = Math.pow(10, ndigits);
      return Math.round(self * scale) / scale;
    
    };

    def.$step = TMP_4 = function(limit, step) {
      var $a, self = this, $iter = TMP_4.$$p, block = $iter || nil;

      if (step == null) {
        step = 1
      }
      TMP_4.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        return self.$enum_for("step", limit, step)
      };
      if ((($a = step == 0) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('ArgumentError'), "step cannot be 0")};
      
      var value = self;

      if (step > 0) {
        while (value <= limit) {
          block(value);
          value += step;
        }
      }
      else {
        while (value >= limit) {
          block(value);
          value += step;
        }
      }
    
      return self;
    };

    Opal.defn(self, '$succ', def.$next);

    def.$times = TMP_5 = function() {
      var self = this, $iter = TMP_5.$$p, block = $iter || nil;

      TMP_5.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        return self.$enum_for("times")
      };
      
      for (var i = 0; i < self; i++) {
        if (block(i) === $breaker) {
          return $breaker.$v;
        }
      }
    
      return self;
    };

    def.$to_f = function() {
      var self = this;

      return self;
    };

    def.$to_i = function() {
      var self = this;

      return parseInt(self);
    };

    Opal.defn(self, '$to_int', def.$to_i);

    def.$to_s = function(base) {
      var $a, $b, self = this;

      if (base == null) {
        base = 10
      }
      if ((($a = ((($b = base['$<'](2)) !== false && $b !== nil) ? $b : base['$>'](36))) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('ArgumentError'), "base must be between 2 and 36")};
      return self.toString(base);
    };

    Opal.defn(self, '$inspect', def.$to_s);

    def.$divmod = function(rhs) {
      var self = this, q = nil, r = nil;

      q = (self['$/'](rhs)).$floor();
      r = self['$%'](rhs);
      return [q, r];
    };

    def.$upto = TMP_6 = function(finish) {
      var self = this, $iter = TMP_6.$$p, block = $iter || nil;

      TMP_6.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        return self.$enum_for("upto", finish)
      };
      
      for (var i = self; i <= finish; i++) {
        if (block(i) === $breaker) {
          return $breaker.$v;
        }
      }
    
      return self;
    };

    def['$zero?'] = function() {
      var self = this;

      return self == 0;
    };

    def.$size = function() {
      var self = this;

      return 4;
    };

    def['$nan?'] = function() {
      var self = this;

      return isNaN(self);
    };

    def['$finite?'] = function() {
      var self = this;

      return self != Infinity && self != -Infinity;
    };

    def['$infinite?'] = function() {
      var self = this;

      
      if (self == Infinity) {
        return +1;
      }
      else if (self == -Infinity) {
        return -1;
      }
      else {
        return nil;
      }
    
    };

    def['$positive?'] = function() {
      var self = this;

      return 1 / self > 0;
    };

    return (def['$negative?'] = function() {
      var self = this;

      return 1 / self < 0;
    }, nil) && 'negative?';
  })(self, null);
  Opal.cdecl($scope, 'Fixnum', $scope.get('Numeric'));
  (function($base, $super) {
    function $Integer(){};
    var self = $Integer = $klass($base, $super, 'Integer', $Integer);

    var def = self.$$proto, $scope = self.$$scope;

    return (Opal.defs(self, '$===', function(other) {
      var self = this;

      
      if (!other.$$is_number) {
        return false;
      }

      return (other % 1) === 0;
    
    }), nil) && '==='
  })(self, $scope.get('Numeric'));
  return (function($base, $super) {
    function $Float(){};
    var self = $Float = $klass($base, $super, 'Float', $Float);

    var def = self.$$proto, $scope = self.$$scope, $a;

    Opal.defs(self, '$===', function(other) {
      var self = this;

      return !!other.$$is_number;
    });

    Opal.cdecl($scope, 'INFINITY', Infinity);

    Opal.cdecl($scope, 'NAN', NaN);

    if ((($a = (typeof(Number.EPSILON) !== "undefined")) !== nil && (!$a.$$is_boolean || $a == true))) {
      return Opal.cdecl($scope, 'EPSILON', Number.EPSILON)
      } else {
      return Opal.cdecl($scope, 'EPSILON', 2.2204460492503130808472633361816E-16)
    };
  })(self, $scope.get('Numeric'));
};

/* Generated by Opal 0.7.2 */
Opal.modules["corelib/complex"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  return (function($base, $super) {
    function $Complex(){};
    var self = $Complex = $klass($base, $super, 'Complex', $Complex);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('Numeric'))
};

/* Generated by Opal 0.7.2 */
Opal.modules["corelib/rational"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  return (function($base, $super) {
    function $Rational(){};
    var self = $Rational = $klass($base, $super, 'Rational', $Rational);

    var def = self.$$proto, $scope = self.$$scope;

    return nil;
  })(self, $scope.get('Numeric'))
};

/* Generated by Opal 0.7.2 */
Opal.modules["corelib/proc"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  Opal.add_stubs(['$raise']);
  return (function($base, $super) {
    function $Proc(){};
    var self = $Proc = $klass($base, $super, 'Proc', $Proc);

    var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_2;

    def.$$is_proc = true;

    def.$$is_lambda = false;

    Opal.defs(self, '$new', TMP_1 = function() {
      var self = this, $iter = TMP_1.$$p, block = $iter || nil;

      TMP_1.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        self.$raise($scope.get('ArgumentError'), "tried to create a Proc object without a block")
      };
      return block;
    });

    def.$call = TMP_2 = function(args) {
      var self = this, $iter = TMP_2.$$p, block = $iter || nil;

      args = $slice.call(arguments, 0);
      TMP_2.$$p = null;
      
      if (block !== nil) {
        self.$$p = block;
      }

      var result;

      if (self.$$is_lambda) {
        result = self.apply(null, args);
      }
      else {
        result = Opal.yieldX(self, args);
      }

      if (result === $breaker) {
        return $breaker.$v;
      }

      return result;
    
    };

    Opal.defn(self, '$[]', def.$call);

    def.$to_proc = function() {
      var self = this;

      return self;
    };

    def['$lambda?'] = function() {
      var self = this;

      return !!self.$$is_lambda;
    };

    return (def.$arity = function() {
      var self = this;

      return self.length;
    }, nil) && 'arity';
  })(self, null)
};

/* Generated by Opal 0.7.2 */
Opal.modules["corelib/method"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  Opal.add_stubs(['$attr_reader', '$class', '$arity', '$new', '$name']);
  (function($base, $super) {
    function $Method(){};
    var self = $Method = $klass($base, $super, 'Method', $Method);

    var def = self.$$proto, $scope = self.$$scope, TMP_1;

    def.method = def.receiver = def.owner = def.name = def.obj = nil;
    self.$attr_reader("owner", "receiver", "name");

    def.$initialize = function(receiver, method, name) {
      var self = this;

      self.receiver = receiver;
      self.owner = receiver.$class();
      self.name = name;
      return self.method = method;
    };

    def.$arity = function() {
      var self = this;

      return self.method.$arity();
    };

    def.$call = TMP_1 = function(args) {
      var self = this, $iter = TMP_1.$$p, block = $iter || nil;

      args = $slice.call(arguments, 0);
      TMP_1.$$p = null;
      
      self.method.$$p = block;

      return self.method.apply(self.receiver, args);
    ;
    };

    Opal.defn(self, '$[]', def.$call);

    def.$unbind = function() {
      var self = this;

      return $scope.get('UnboundMethod').$new(self.owner, self.method, self.name);
    };

    def.$to_proc = function() {
      var self = this;

      return self.method;
    };

    return (def.$inspect = function() {
      var self = this;

      return "#<Method: " + (self.obj.$class()) + "#" + (self.name) + "}>";
    }, nil) && 'inspect';
  })(self, null);
  return (function($base, $super) {
    function $UnboundMethod(){};
    var self = $UnboundMethod = $klass($base, $super, 'UnboundMethod', $UnboundMethod);

    var def = self.$$proto, $scope = self.$$scope;

    def.method = def.name = def.owner = nil;
    self.$attr_reader("owner", "name");

    def.$initialize = function(owner, method, name) {
      var self = this;

      self.owner = owner;
      self.method = method;
      return self.name = name;
    };

    def.$arity = function() {
      var self = this;

      return self.method.$arity();
    };

    def.$bind = function(object) {
      var self = this;

      return $scope.get('Method').$new(object, self.method, self.name);
    };

    return (def.$inspect = function() {
      var self = this;

      return "#<UnboundMethod: " + (self.owner.$name()) + "#" + (self.name) + ">";
    }, nil) && 'inspect';
  })(self, null);
};

/* Generated by Opal 0.7.2 */
Opal.modules["corelib/range"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  Opal.add_stubs(['$require', '$include', '$attr_reader', '$<=>', '$raise', '$include?', '$<=', '$<', '$enum_for', '$succ', '$!', '$==', '$===', '$exclude_end?', '$eql?', '$begin', '$end', '$-', '$abs', '$to_i', '$inspect']);
  self.$require("corelib/enumerable");
  return (function($base, $super) {
    function $Range(){};
    var self = $Range = $klass($base, $super, 'Range', $Range);

    var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_2, TMP_3;

    def.begin = def.exclude = def.end = nil;
    self.$include($scope.get('Enumerable'));

    def.$$is_range = true;

    self.$attr_reader("begin", "end");

    def.$initialize = function(first, last, exclude) {
      var $a, self = this;

      if (exclude == null) {
        exclude = false
      }
      if ((($a = first['$<=>'](last)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise($scope.get('ArgumentError'))
      };
      self.begin = first;
      self.end = last;
      return self.exclude = exclude;
    };

    def['$=='] = function(other) {
      var self = this;

      
      if (!other.$$is_range) {
        return false;
      }

      return self.exclude === other.exclude &&
             self.begin   ==  other.begin &&
             self.end     ==  other.end;
    
    };

    def['$==='] = function(value) {
      var self = this;

      return self['$include?'](value);
    };

    def['$cover?'] = function(value) {
      var $a, $b, self = this;

      return (($a = self.begin['$<='](value)) ? ((function() {if ((($b = self.exclude) !== nil && (!$b.$$is_boolean || $b == true))) {
        return value['$<'](self.end)
        } else {
        return value['$<='](self.end)
      }; return nil; })()) : $a);
    };

    def.$each = TMP_1 = function() {
      var $a, $b, self = this, $iter = TMP_1.$$p, block = $iter || nil, current = nil, last = nil;

      TMP_1.$$p = null;
      if ((block !== nil)) {
        } else {
        return self.$enum_for("each")
      };
      current = self.begin;
      last = self.end;
      while (current['$<'](last)) {
      if (Opal.yield1(block, current) === $breaker) return $breaker.$v;
      current = current.$succ();};
      if ((($a = ($b = self.exclude['$!'](), $b !== false && $b !== nil ?current['$=='](last) : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
        if (Opal.yield1(block, current) === $breaker) return $breaker.$v};
      return self;
    };

    def['$eql?'] = function(other) {
      var $a, $b, self = this;

      if ((($a = $scope.get('Range')['$==='](other)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        return false
      };
      return ($a = ($b = self.exclude['$==='](other['$exclude_end?']()), $b !== false && $b !== nil ?self.begin['$eql?'](other.$begin()) : $b), $a !== false && $a !== nil ?self.end['$eql?'](other.$end()) : $a);
    };

    def['$exclude_end?'] = function() {
      var self = this;

      return self.exclude;
    };

    Opal.defn(self, '$first', def.$begin);

    Opal.defn(self, '$include?', def['$cover?']);

    Opal.defn(self, '$last', def.$end);

    def.$max = TMP_2 = function() {var $zuper = $slice.call(arguments, 0);
      var self = this, $iter = TMP_2.$$p, $yield = $iter || nil;

      TMP_2.$$p = null;
      if (($yield !== nil)) {
        return Opal.find_super_dispatcher(self, 'max', TMP_2, $iter).apply(self, $zuper)
        } else {
        return self.exclude ? self.end - 1 : self.end;
      };
    };

    Opal.defn(self, '$member?', def['$cover?']);

    def.$min = TMP_3 = function() {var $zuper = $slice.call(arguments, 0);
      var self = this, $iter = TMP_3.$$p, $yield = $iter || nil;

      TMP_3.$$p = null;
      if (($yield !== nil)) {
        return Opal.find_super_dispatcher(self, 'min', TMP_3, $iter).apply(self, $zuper)
        } else {
        return self.begin
      };
    };

    Opal.defn(self, '$member?', def['$include?']);

    def.$size = function() {
      var $a, $b, self = this, _begin = nil, _end = nil, infinity = nil;

      _begin = self.begin;
      _end = self.end;
      if ((($a = self.exclude) !== nil && (!$a.$$is_boolean || $a == true))) {
        _end = _end['$-'](1)};
      if ((($a = ($b = $scope.get('Numeric')['$==='](_begin), $b !== false && $b !== nil ?$scope.get('Numeric')['$==='](_end) : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        return nil
      };
      if (_end['$<'](_begin)) {
        return 0};
      infinity = (($scope.get('Float')).$$scope.get('INFINITY'));
      if ((($a = ((($b = infinity['$=='](_begin.$abs())) !== false && $b !== nil) ? $b : _end.$abs()['$=='](infinity))) !== nil && (!$a.$$is_boolean || $a == true))) {
        return infinity};
      return ((Math.abs(_end - _begin) + 1)).$to_i();
    };

    def.$step = function(n) {
      var self = this;

      if (n == null) {
        n = 1
      }
      return self.$raise($scope.get('NotImplementedError'));
    };

    def.$to_s = function() {
      var self = this;

      return self.begin.$inspect() + (self.exclude ? '...' : '..') + self.end.$inspect();
    };

    return Opal.defn(self, '$inspect', def.$to_s);
  })(self, null);
};

/* Generated by Opal 0.7.2 */
Opal.modules["corelib/time"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass, $range = Opal.range;

  Opal.add_stubs(['$require', '$include', '$kind_of?', '$to_i', '$coerce_to', '$between?', '$raise', '$new', '$compact', '$nil?', '$===', '$<=>', '$to_f', '$strftime', '$is_a?', '$zero?', '$wday', '$utc?', '$warn', '$year', '$mon', '$day', '$yday', '$hour', '$min', '$sec', '$rjust', '$ljust', '$zone', '$to_s', '$[]', '$cweek_cyear', '$month', '$isdst', '$private', '$<=', '$!', '$==', '$-', '$ceil', '$/', '$+']);
  self.$require("corelib/comparable");
  return (function($base, $super) {
    function $Time(){};
    var self = $Time = $klass($base, $super, 'Time', $Time);

    var def = self.$$proto, $scope = self.$$scope;

    def.tz_offset = nil;
    self.$include($scope.get('Comparable'));

    
    var days_of_week = ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"],
        short_days   = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"],
        short_months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"],
        long_months  = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
  ;

    Opal.defs(self, '$at', function(seconds, frac) {
      var self = this;

      if (frac == null) {
        frac = 0
      }
      return new Date(seconds * 1000 + frac);
    });

    Opal.defs(self, '$new', function(year, month, day, hour, minute, second, utc_offset) {
      var self = this;

      
      switch (arguments.length) {
        case 1:
          return new Date(year, 0);

        case 2:
          return new Date(year, month - 1);

        case 3:
          return new Date(year, month - 1, day);

        case 4:
          return new Date(year, month - 1, day, hour);

        case 5:
          return new Date(year, month - 1, day, hour, minute);

        case 6:
          return new Date(year, month - 1, day, hour, minute, second);

        case 7:
          return new Date(year, month - 1, day, hour, minute, second);

        default:
          return new Date();
      }
    
    });

    Opal.defs(self, '$local', function(year, month, day, hour, minute, second, millisecond) {
      var $a, self = this;

      if (month == null) {
        month = nil
      }
      if (day == null) {
        day = nil
      }
      if (hour == null) {
        hour = nil
      }
      if (minute == null) {
        minute = nil
      }
      if (second == null) {
        second = nil
      }
      if (millisecond == null) {
        millisecond = nil
      }
      if ((($a = arguments.length === 10) !== nil && (!$a.$$is_boolean || $a == true))) {
        
        var args = $slice.call(arguments).reverse();

        second = args[9];
        minute = args[8];
        hour   = args[7];
        day    = args[6];
        month  = args[5];
        year   = args[4];
      };
      year = (function() {if ((($a = year['$kind_of?']($scope.get('String'))) !== nil && (!$a.$$is_boolean || $a == true))) {
        return year.$to_i()
        } else {
        return $scope.get('Opal').$coerce_to(year, $scope.get('Integer'), "to_int")
      }; return nil; })();
      month = (function() {if ((($a = month['$kind_of?']($scope.get('String'))) !== nil && (!$a.$$is_boolean || $a == true))) {
        return month.$to_i()
        } else {
        return $scope.get('Opal').$coerce_to(((($a = month) !== false && $a !== nil) ? $a : 1), $scope.get('Integer'), "to_int")
      }; return nil; })();
      if ((($a = month['$between?'](1, 12)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise($scope.get('ArgumentError'), "month out of range: " + (month))
      };
      day = (function() {if ((($a = day['$kind_of?']($scope.get('String'))) !== nil && (!$a.$$is_boolean || $a == true))) {
        return day.$to_i()
        } else {
        return $scope.get('Opal').$coerce_to(((($a = day) !== false && $a !== nil) ? $a : 1), $scope.get('Integer'), "to_int")
      }; return nil; })();
      if ((($a = day['$between?'](1, 31)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise($scope.get('ArgumentError'), "day out of range: " + (day))
      };
      hour = (function() {if ((($a = hour['$kind_of?']($scope.get('String'))) !== nil && (!$a.$$is_boolean || $a == true))) {
        return hour.$to_i()
        } else {
        return $scope.get('Opal').$coerce_to(((($a = hour) !== false && $a !== nil) ? $a : 0), $scope.get('Integer'), "to_int")
      }; return nil; })();
      if ((($a = hour['$between?'](0, 24)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise($scope.get('ArgumentError'), "hour out of range: " + (hour))
      };
      minute = (function() {if ((($a = minute['$kind_of?']($scope.get('String'))) !== nil && (!$a.$$is_boolean || $a == true))) {
        return minute.$to_i()
        } else {
        return $scope.get('Opal').$coerce_to(((($a = minute) !== false && $a !== nil) ? $a : 0), $scope.get('Integer'), "to_int")
      }; return nil; })();
      if ((($a = minute['$between?'](0, 59)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise($scope.get('ArgumentError'), "minute out of range: " + (minute))
      };
      second = (function() {if ((($a = second['$kind_of?']($scope.get('String'))) !== nil && (!$a.$$is_boolean || $a == true))) {
        return second.$to_i()
        } else {
        return $scope.get('Opal').$coerce_to(((($a = second) !== false && $a !== nil) ? $a : 0), $scope.get('Integer'), "to_int")
      }; return nil; })();
      if ((($a = second['$between?'](0, 59)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise($scope.get('ArgumentError'), "second out of range: " + (second))
      };
      return ($a = self).$new.apply($a, [].concat([year, month, day, hour, minute, second].$compact()));
    });

    Opal.defs(self, '$gm', function(year, month, day, hour, minute, second, utc_offset) {
      var $a, self = this;

      if ((($a = year['$nil?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('TypeError'), "missing year (got nil)")};
      
      if (month > 12 || day > 31 || hour > 24 || minute > 59 || second > 59) {
        self.$raise($scope.get('ArgumentError'));
      }

      var date = new Date(Date.UTC(year, (month || 1) - 1, (day || 1), (hour || 0), (minute || 0), (second || 0)));
      date.tz_offset = 0
      return date;
    ;
    });

    (function(self) {
      var $scope = self.$$scope, def = self.$$proto;

      self.$$proto.$mktime = self.$$proto.$local;
      return self.$$proto.$utc = self.$$proto.$gm;
    })(self.$singleton_class());

    Opal.defs(self, '$now', function() {
      var self = this;

      return new Date();
    });

    def['$+'] = function(other) {
      var $a, self = this;

      if ((($a = $scope.get('Time')['$==='](other)) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('TypeError'), "time + time?")};
      other = $scope.get('Opal').$coerce_to(other, $scope.get('Integer'), "to_int");
      
      var result           = new Date(self.getTime() + (other * 1000));
          result.tz_offset = self.tz_offset;

      return result;
    
    };

    def['$-'] = function(other) {
      var $a, self = this;

      if ((($a = $scope.get('Time')['$==='](other)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return (self.getTime() - other.getTime()) / 1000};
      other = $scope.get('Opal').$coerce_to(other, $scope.get('Integer'), "to_int");
      
      var result           = new Date(self.getTime() - (other * 1000));
          result.tz_offset = self.tz_offset;

      return result;
    
    };

    def['$<=>'] = function(other) {
      var self = this;

      return self.$to_f()['$<=>'](other.$to_f());
    };

    def['$=='] = function(other) {
      var self = this;

      return self.$to_f() === other.$to_f();
    };

    def.$asctime = function() {
      var self = this;

      return self.$strftime("%a %b %e %H:%M:%S %Y");
    };

    Opal.defn(self, '$ctime', def.$asctime);

    def.$day = function() {
      var self = this;

      
      if (self.tz_offset === 0) {
        return self.getUTCDate();
      }
      else {
        return self.getDate();
      }
    ;
    };

    def.$yday = function() {
      var self = this;

      
      // http://javascript.about.com/library/bldayyear.htm
      var onejan = new Date(self.getFullYear(), 0, 1);
      return Math.ceil((self - onejan) / 86400000);
    
    };

    def.$isdst = function() {
      var self = this;

      return self.$raise($scope.get('NotImplementedError'));
    };

    def['$eql?'] = function(other) {
      var $a, self = this;

      return ($a = other['$is_a?']($scope.get('Time')), $a !== false && $a !== nil ?(self['$<=>'](other))['$zero?']() : $a);
    };

    def['$friday?'] = function() {
      var self = this;

      return self.$wday() == 5;
    };

    def.$hour = function() {
      var self = this;

      
      if (self.tz_offset === 0) {
        return self.getUTCHours();
      }
      else {
        return self.getHours();
      }
    ;
    };

    def.$inspect = function() {
      var $a, self = this;

      if ((($a = self['$utc?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self.$strftime("%Y-%m-%d %H:%M:%S UTC")
        } else {
        return self.$strftime("%Y-%m-%d %H:%M:%S %z")
      };
    };

    Opal.defn(self, '$mday', def.$day);

    def.$min = function() {
      var self = this;

      
      if (self.tz_offset === 0) {
        return self.getUTCMinutes();
      }
      else {
        return self.getMinutes();
      }
    ;
    };

    def.$mon = function() {
      var self = this;

      
      if (self.tz_offset === 0) {
        return self.getUTCMonth() + 1;
      }
      else {
        return self.getMonth() + 1;
      }
    ;
    };

    def['$monday?'] = function() {
      var self = this;

      return self.$wday() == 1;
    };

    Opal.defn(self, '$month', def.$mon);

    def['$saturday?'] = function() {
      var self = this;

      return self.$wday() == 6;
    };

    def.$sec = function() {
      var self = this;

      
      if (self.tz_offset === 0) {
        return self.getUTCSeconds();
      }
      else {
        return self.getSeconds();
      }
    ;
    };

    def.$usec = function() {
      var self = this;

      self.$warn("Microseconds are not supported");
      return 0;
    };

    def.$zone = function() {
      var self = this;

      
      var string = self.toString(),
          result;

      if (string.indexOf('(') == -1) {
        result = string.match(/[A-Z]{3,4}/)[0];
      }
      else {
        result = string.match(/\([^)]+\)/)[0].match(/[A-Z]/g).join('');
      }

      if (result == "GMT" && /(GMT\W*\d{4})/.test(string)) {
        return RegExp.$1;
      }
      else {
        return result;
      }
    
    };

    def.$getgm = function() {
      var self = this;

      
      var result           = new Date(self.getTime());
          result.tz_offset = 0;

      return result;
    
    };

    def['$gmt?'] = function() {
      var self = this;

      return self.tz_offset === 0;
    };

    def.$gmt_offset = function() {
      var self = this;

      return -self.getTimezoneOffset() * 60;
    };

    def.$strftime = function(format) {
      var self = this;

      
      return format.replace(/%([\-_#^0]*:{0,2})(\d+)?([EO]*)(.)/g, function(full, flags, width, _, conv) {
        var result = "",
            width  = parseInt(width),
            zero   = flags.indexOf('0') !== -1,
            pad    = flags.indexOf('-') === -1,
            blank  = flags.indexOf('_') !== -1,
            upcase = flags.indexOf('^') !== -1,
            invert = flags.indexOf('#') !== -1,
            colons = (flags.match(':') || []).length;

        if (zero && blank) {
          if (flags.indexOf('0') < flags.indexOf('_')) {
            zero = false;
          }
          else {
            blank = false;
          }
        }

        switch (conv) {
          case 'Y':
            result += self.$year();
            break;

          case 'C':
            zero    = !blank;
            result += Math.round(self.$year() / 100);
            break;

          case 'y':
            zero    = !blank;
            result += (self.$year() % 100);
            break;

          case 'm':
            zero    = !blank;
            result += self.$mon();
            break;

          case 'B':
            result += long_months[self.$mon() - 1];
            break;

          case 'b':
          case 'h':
            blank   = !zero;
            result += short_months[self.$mon() - 1];
            break;

          case 'd':
            zero    = !blank
            result += self.$day();
            break;

          case 'e':
            blank   = !zero
            result += self.$day();
            break;

          case 'j':
            result += self.$yday();
            break;

          case 'H':
            zero    = !blank;
            result += self.$hour();
            break;

          case 'k':
            blank   = !zero;
            result += self.$hour();
            break;

          case 'I':
            zero    = !blank;
            result += (self.$hour() % 12 || 12);
            break;

          case 'l':
            blank   = !zero;
            result += (self.$hour() % 12 || 12);
            break;

          case 'P':
            result += (self.$hour() >= 12 ? "pm" : "am");
            break;

          case 'p':
            result += (self.$hour() >= 12 ? "PM" : "AM");
            break;

          case 'M':
            zero    = !blank;
            result += self.$min();
            break;

          case 'S':
            zero    = !blank;
            result += self.$sec()
            break;

          case 'L':
            zero    = !blank;
            width   = isNaN(width) ? 3 : width;
            result += self.getMilliseconds();
            break;

          case 'N':
            width   = isNaN(width) ? 9 : width;
            result += (self.getMilliseconds().toString()).$rjust(3, "0");
            result  = (result).$ljust(width, "0");
            break;

          case 'z':
            var offset  = self.getTimezoneOffset(),
                hours   = Math.floor(Math.abs(offset) / 60),
                minutes = Math.abs(offset) % 60;

            result += offset < 0 ? "+" : "-";
            result += hours < 10 ? "0" : "";
            result += hours;

            if (colons > 0) {
              result += ":";
            }

            result += minutes < 10 ? "0" : "";
            result += minutes;

            if (colons > 1) {
              result += ":00";
            }

            break;

          case 'Z':
            result += self.$zone();
            break;

          case 'A':
            result += days_of_week[self.$wday()];
            break;

          case 'a':
            result += short_days[self.$wday()];
            break;

          case 'u':
            result += (self.$wday() + 1);
            break;

          case 'w':
            result += self.$wday();
            break;

          case 'V':
            result += self.$cweek_cyear()['$[]'](0).$to_s().$rjust(2, "0");
            break;

          case 'G':
            result += self.$cweek_cyear()['$[]'](1);
            break;

          case 'g':
            result += self.$cweek_cyear()['$[]'](1)['$[]']($range(-2, -1, false));
            break;

          case 's':
            result += self.$to_i();
            break;

          case 'n':
            result += "\n";
            break;

          case 't':
            result += "\t";
            break;

          case '%':
            result += "%";
            break;

          case 'c':
            result += self.$strftime("%a %b %e %T %Y");
            break;

          case 'D':
          case 'x':
            result += self.$strftime("%m/%d/%y");
            break;

          case 'F':
            result += self.$strftime("%Y-%m-%d");
            break;

          case 'v':
            result += self.$strftime("%e-%^b-%4Y");
            break;

          case 'r':
            result += self.$strftime("%I:%M:%S %p");
            break;

          case 'R':
            result += self.$strftime("%H:%M");
            break;

          case 'T':
          case 'X':
            result += self.$strftime("%H:%M:%S");
            break;

          default:
            return full;
        }

        if (upcase) {
          result = result.toUpperCase();
        }

        if (invert) {
          result = result.replace(/[A-Z]/, function(c) { c.toLowerCase() }).
                          replace(/[a-z]/, function(c) { c.toUpperCase() });
        }

        if (pad && (zero || blank)) {
          result = (result).$rjust(isNaN(width) ? 2 : width, blank ? " " : "0");
        }

        return result;
      });
    
    };

    def['$sunday?'] = function() {
      var self = this;

      return self.$wday() == 0;
    };

    def['$thursday?'] = function() {
      var self = this;

      return self.$wday() == 4;
    };

    def.$to_a = function() {
      var self = this;

      return [self.$sec(), self.$min(), self.$hour(), self.$day(), self.$month(), self.$year(), self.$wday(), self.$yday(), self.$isdst(), self.$zone()];
    };

    def.$to_f = function() {
      var self = this;

      return self.getTime() / 1000;
    };

    def.$to_i = function() {
      var self = this;

      return parseInt(self.getTime() / 1000);
    };

    Opal.defn(self, '$to_s', def.$inspect);

    def['$tuesday?'] = function() {
      var self = this;

      return self.$wday() == 2;
    };

    Opal.defn(self, '$utc?', def['$gmt?']);

    Opal.defn(self, '$utc_offset', def.$gmt_offset);

    def.$wday = function() {
      var self = this;

      
      if (self.tz_offset === 0) {
        return self.getUTCDay();
      }
      else {
        return self.getDay();
      }
    ;
    };

    def['$wednesday?'] = function() {
      var self = this;

      return self.$wday() == 3;
    };

    def.$year = function() {
      var self = this;

      
      if (self.tz_offset === 0) {
        return self.getUTCFullYear();
      }
      else {
        return self.getFullYear();
      }
    ;
    };

    self.$private("cweek_cyear");

    return (def.$cweek_cyear = function() {
      var $a, $b, self = this, jan01 = nil, jan01_wday = nil, first_monday = nil, year = nil, offset = nil, week = nil, dec31 = nil, dec31_wday = nil;

      jan01 = $scope.get('Time').$new(self.$year(), 1, 1);
      jan01_wday = jan01.$wday();
      first_monday = 0;
      year = self.$year();
      if ((($a = (($b = jan01_wday['$<='](4)) ? jan01_wday['$=='](0)['$!']() : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
        offset = jan01_wday['$-'](1)
        } else {
        offset = jan01_wday['$-'](7)['$-'](1);
        if (offset['$=='](-8)) {
          offset = -1};
      };
      week = ((self.$yday()['$+'](offset))['$/'](7.0)).$ceil();
      if (week['$<='](0)) {
        return $scope.get('Time').$new(self.$year()['$-'](1), 12, 31).$cweek_cyear()
      } else if (week['$=='](53)) {
        dec31 = $scope.get('Time').$new(self.$year(), 12, 31);
        dec31_wday = dec31.$wday();
        if ((($a = (($b = dec31_wday['$<='](3)) ? dec31_wday['$=='](0)['$!']() : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
          week = 1;
          year = year['$+'](1);};};
      return [week, year];
    }, nil) && 'cweek_cyear';
  })(self, null);
};

/* Generated by Opal 0.7.2 */
Opal.modules["corelib/struct"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  Opal.add_stubs(['$==', '$[]', '$upcase', '$const_set', '$new', '$unshift', '$each', '$define_struct_attribute', '$instance_eval', '$to_proc', '$raise', '$<<', '$members', '$attr_accessor', '$include', '$each_with_index', '$instance_variable_set', '$class', '$===', '$>=', '$size', '$include?', '$to_sym', '$instance_variable_get', '$enum_for', '$hash', '$all?', '$length', '$map', '$+', '$join', '$inspect', '$each_pair']);
  return (function($base, $super) {
    function $Struct(){};
    var self = $Struct = $klass($base, $super, 'Struct', $Struct);

    var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_6, TMP_8;

    Opal.defs(self, '$new', TMP_1 = function(name, args) {var $zuper = $slice.call(arguments, 0);
      var $a, $b, $c, TMP_2, self = this, $iter = TMP_1.$$p, block = $iter || nil;

      args = $slice.call(arguments, 1);
      TMP_1.$$p = null;
      if (self['$==']($scope.get('Struct'))) {
        } else {
        return Opal.find_super_dispatcher(self, 'new', TMP_1, $iter, $Struct).apply(self, $zuper)
      };
      if (name['$[]'](0)['$=='](name['$[]'](0).$upcase())) {
        return $scope.get('Struct').$const_set(name, ($a = self).$new.apply($a, [].concat(args)))
        } else {
        args.$unshift(name);
        return ($b = ($c = $scope.get('Class')).$new, $b.$$p = (TMP_2 = function(){var self = TMP_2.$$s || this, $a, $b, TMP_3, $c;

        ($a = ($b = args).$each, $a.$$p = (TMP_3 = function(arg){var self = TMP_3.$$s || this;
if (arg == null) arg = nil;
          return self.$define_struct_attribute(arg)}, TMP_3.$$s = self, TMP_3), $a).call($b);
          if (block !== false && block !== nil) {
            return ($a = ($c = self).$instance_eval, $a.$$p = block.$to_proc(), $a).call($c)
            } else {
            return nil
          };}, TMP_2.$$s = self, TMP_2), $b).call($c, self);
      };
    });

    Opal.defs(self, '$define_struct_attribute', function(name) {
      var self = this;

      if (self['$==']($scope.get('Struct'))) {
        self.$raise($scope.get('ArgumentError'), "you cannot define attributes to the Struct class")};
      self.$members()['$<<'](name);
      return self.$attr_accessor(name);
    });

    Opal.defs(self, '$members', function() {
      var $a, self = this;
      if (self.members == null) self.members = nil;

      if (self['$==']($scope.get('Struct'))) {
        self.$raise($scope.get('ArgumentError'), "the Struct class has no members")};
      return ((($a = self.members) !== false && $a !== nil) ? $a : self.members = []);
    });

    Opal.defs(self, '$inherited', function(klass) {
      var $a, $b, TMP_4, self = this, members = nil;
      if (self.members == null) self.members = nil;

      if (self['$==']($scope.get('Struct'))) {
        return nil};
      members = self.members;
      return ($a = ($b = klass).$instance_eval, $a.$$p = (TMP_4 = function(){var self = TMP_4.$$s || this;

      return self.members = members}, TMP_4.$$s = self, TMP_4), $a).call($b);
    });

    (function(self) {
      var $scope = self.$$scope, def = self.$$proto;

      return self.$$proto['$[]'] = self.$$proto.$new
    })(self.$singleton_class());

    self.$include($scope.get('Enumerable'));

    def.$initialize = function(args) {
      var $a, $b, TMP_5, self = this;

      args = $slice.call(arguments, 0);
      return ($a = ($b = self.$members()).$each_with_index, $a.$$p = (TMP_5 = function(name, index){var self = TMP_5.$$s || this;
if (name == null) name = nil;if (index == null) index = nil;
      return self.$instance_variable_set("@" + (name), args['$[]'](index))}, TMP_5.$$s = self, TMP_5), $a).call($b);
    };

    def.$members = function() {
      var self = this;

      return self.$class().$members();
    };

    def['$[]'] = function(name) {
      var $a, self = this;

      if ((($a = $scope.get('Integer')['$==='](name)) !== nil && (!$a.$$is_boolean || $a == true))) {
        if (name['$>='](self.$members().$size())) {
          self.$raise($scope.get('IndexError'), "offset " + (name) + " too large for struct(size:" + (self.$members().$size()) + ")")};
        name = self.$members()['$[]'](name);
      } else if ((($a = self.$members()['$include?'](name.$to_sym())) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise($scope.get('NameError'), "no member '" + (name) + "' in struct")
      };
      return self.$instance_variable_get("@" + (name));
    };

    def['$[]='] = function(name, value) {
      var $a, self = this;

      if ((($a = $scope.get('Integer')['$==='](name)) !== nil && (!$a.$$is_boolean || $a == true))) {
        if (name['$>='](self.$members().$size())) {
          self.$raise($scope.get('IndexError'), "offset " + (name) + " too large for struct(size:" + (self.$members().$size()) + ")")};
        name = self.$members()['$[]'](name);
      } else if ((($a = self.$members()['$include?'](name.$to_sym())) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        self.$raise($scope.get('NameError'), "no member '" + (name) + "' in struct")
      };
      return self.$instance_variable_set("@" + (name), value);
    };

    def.$each = TMP_6 = function() {
      var $a, $b, TMP_7, self = this, $iter = TMP_6.$$p, $yield = $iter || nil;

      TMP_6.$$p = null;
      if (($yield !== nil)) {
        } else {
        return self.$enum_for("each")
      };
      ($a = ($b = self.$members()).$each, $a.$$p = (TMP_7 = function(name){var self = TMP_7.$$s || this, $a;
if (name == null) name = nil;
      return $a = Opal.yield1($yield, self['$[]'](name)), $a === $breaker ? $a : $a}, TMP_7.$$s = self, TMP_7), $a).call($b);
      return self;
    };

    def.$each_pair = TMP_8 = function() {
      var $a, $b, TMP_9, self = this, $iter = TMP_8.$$p, $yield = $iter || nil;

      TMP_8.$$p = null;
      if (($yield !== nil)) {
        } else {
        return self.$enum_for("each_pair")
      };
      ($a = ($b = self.$members()).$each, $a.$$p = (TMP_9 = function(name){var self = TMP_9.$$s || this, $a;
if (name == null) name = nil;
      return $a = Opal.yieldX($yield, [name, self['$[]'](name)]), $a === $breaker ? $a : $a}, TMP_9.$$s = self, TMP_9), $a).call($b);
      return self;
    };

    def['$eql?'] = function(other) {
      var $a, $b, $c, TMP_10, self = this;

      return ((($a = self.$hash()['$=='](other.$hash())) !== false && $a !== nil) ? $a : ($b = ($c = other.$each_with_index())['$all?'], $b.$$p = (TMP_10 = function(object, index){var self = TMP_10.$$s || this;
if (object == null) object = nil;if (index == null) index = nil;
      return self['$[]'](self.$members()['$[]'](index))['$=='](object)}, TMP_10.$$s = self, TMP_10), $b).call($c));
    };

    def.$length = function() {
      var self = this;

      return self.$members().$length();
    };

    Opal.defn(self, '$size', def.$length);

    def.$to_a = function() {
      var $a, $b, TMP_11, self = this;

      return ($a = ($b = self.$members()).$map, $a.$$p = (TMP_11 = function(name){var self = TMP_11.$$s || this;
if (name == null) name = nil;
      return self['$[]'](name)}, TMP_11.$$s = self, TMP_11), $a).call($b);
    };

    Opal.defn(self, '$values', def.$to_a);

    def.$inspect = function() {
      var $a, $b, TMP_12, self = this, result = nil;

      result = "#<struct ";
      if (self.$class()['$==']($scope.get('Struct'))) {
        result = result['$+']("" + (self.$class()) + " ")};
      result = result['$+'](($a = ($b = self.$each_pair()).$map, $a.$$p = (TMP_12 = function(name, value){var self = TMP_12.$$s || this;
if (name == null) name = nil;if (value == null) value = nil;
      return "" + (name) + "=" + (value.$inspect())}, TMP_12.$$s = self, TMP_12), $a).call($b).$join(", "));
      result = result['$+'](">");
      return result;
    };

    return Opal.defn(self, '$to_s', def.$inspect);
  })(self, null)
};

/* Generated by Opal 0.7.2 */
Opal.modules["corelib/io"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var $a, $b, self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass, $module = Opal.module, $gvars = Opal.gvars;
  if ($gvars.stdout == null) $gvars.stdout = nil;
  if ($gvars.stderr == null) $gvars.stderr = nil;

  Opal.add_stubs(['$attr_accessor', '$size', '$write', '$join', '$map', '$String', '$empty?', '$concat', '$chomp', '$getbyte', '$getc', '$raise', '$new', '$write_proc=', '$extend']);
  (function($base, $super) {
    function $IO(){};
    var self = $IO = $klass($base, $super, 'IO', $IO);

    var def = self.$$proto, $scope = self.$$scope;

    def.tty = def.closed = nil;
    Opal.cdecl($scope, 'SEEK_SET', 0);

    Opal.cdecl($scope, 'SEEK_CUR', 1);

    Opal.cdecl($scope, 'SEEK_END', 2);

    def['$tty?'] = function() {
      var self = this;

      return self.tty;
    };

    def['$closed?'] = function() {
      var self = this;

      return self.closed;
    };

    self.$attr_accessor("write_proc");

    def.$write = function(string) {
      var self = this;

      self.write_proc(string);
      return string.$size();
    };

    self.$attr_accessor("sync");

    (function($base) {
      var self = $module($base, 'Writable');

      var def = self.$$proto, $scope = self.$$scope;

      Opal.defn(self, '$<<', function(string) {
        var self = this;

        self.$write(string);
        return self;
      });

      Opal.defn(self, '$print', function(args) {
        var $a, $b, TMP_1, self = this;
        if ($gvars[","] == null) $gvars[","] = nil;

        args = $slice.call(arguments, 0);
        self.$write(($a = ($b = args).$map, $a.$$p = (TMP_1 = function(arg){var self = TMP_1.$$s || this;
if (arg == null) arg = nil;
        return self.$String(arg)}, TMP_1.$$s = self, TMP_1), $a).call($b).$join($gvars[","]));
        return nil;
      });

      Opal.defn(self, '$puts', function(args) {
        var $a, $b, TMP_2, self = this, newline = nil;
        if ($gvars["/"] == null) $gvars["/"] = nil;

        args = $slice.call(arguments, 0);
        newline = $gvars["/"];
        if ((($a = args['$empty?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
          self.$write($gvars["/"])
          } else {
          self.$write(($a = ($b = args).$map, $a.$$p = (TMP_2 = function(arg){var self = TMP_2.$$s || this;
if (arg == null) arg = nil;
          return self.$String(arg).$chomp()}, TMP_2.$$s = self, TMP_2), $a).call($b).$concat([nil]).$join(newline))
        };
        return nil;
      });
    })(self);

    return (function($base) {
      var self = $module($base, 'Readable');

      var def = self.$$proto, $scope = self.$$scope;

      Opal.defn(self, '$readbyte', function() {
        var self = this;

        return self.$getbyte();
      });

      Opal.defn(self, '$readchar', function() {
        var self = this;

        return self.$getc();
      });

      Opal.defn(self, '$readline', function(sep) {
        var self = this;
        if ($gvars["/"] == null) $gvars["/"] = nil;

        if (sep == null) {
          sep = $gvars["/"]
        }
        return self.$raise($scope.get('NotImplementedError'));
      });

      Opal.defn(self, '$readpartial', function(integer, outbuf) {
        var self = this;

        if (outbuf == null) {
          outbuf = nil
        }
        return self.$raise($scope.get('NotImplementedError'));
      });
    })(self);
  })(self, null);
  Opal.cdecl($scope, 'STDERR', $gvars.stderr = $scope.get('IO').$new());
  Opal.cdecl($scope, 'STDIN', $gvars.stdin = $scope.get('IO').$new());
  Opal.cdecl($scope, 'STDOUT', $gvars.stdout = $scope.get('IO').$new());
  (($a = [typeof(process) === 'object' ? function(s){process.stdout.write(s)} : function(s){console.log(s)}]), $b = $gvars.stdout, $b['$write_proc='].apply($b, $a), $a[$a.length-1]);
  (($a = [typeof(process) === 'object' ? function(s){process.stderr.write(s)} : function(s){console.warn(s)}]), $b = $gvars.stderr, $b['$write_proc='].apply($b, $a), $a[$a.length-1]);
  $gvars.stdout.$extend((($scope.get('IO')).$$scope.get('Writable')));
  return $gvars.stderr.$extend((($scope.get('IO')).$$scope.get('Writable')));
};

/* Generated by Opal 0.7.2 */
Opal.modules["corelib/main"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice;

  Opal.add_stubs(['$include']);
  Opal.defs(self, '$to_s', function() {
    var self = this;

    return "main";
  });
  return (Opal.defs(self, '$include', function(mod) {
    var self = this;

    return $scope.get('Object').$include(mod);
  }), nil) && 'include';
};

/* Generated by Opal 0.7.2 */
Opal.modules["corelib/variables"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $gvars = Opal.gvars, $hash2 = Opal.hash2;

  Opal.add_stubs(['$new']);
  $gvars["&"] = $gvars["~"] = $gvars["`"] = $gvars["'"] = nil;
  $gvars.LOADED_FEATURES = $gvars["\""] = Opal.loaded_features;
  $gvars.LOAD_PATH = $gvars[":"] = [];
  $gvars["/"] = "\n";
  $gvars[","] = nil;
  Opal.cdecl($scope, 'ARGV', []);
  Opal.cdecl($scope, 'ARGF', $scope.get('Object').$new());
  Opal.cdecl($scope, 'ENV', $hash2([], {}));
  $gvars.VERBOSE = false;
  $gvars.DEBUG = false;
  $gvars.SAFE = 0;
  Opal.cdecl($scope, 'RUBY_PLATFORM', "opal");
  Opal.cdecl($scope, 'RUBY_ENGINE', "opal");
  Opal.cdecl($scope, 'RUBY_VERSION', "2.1.1");
  Opal.cdecl($scope, 'RUBY_ENGINE_VERSION', "0.7.2");
  return Opal.cdecl($scope, 'RUBY_RELEASE_DATE', "2014-04-15");
};

/* Generated by Opal 0.7.2 */
Opal.modules["corelib/dir"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  Opal.add_stubs(['$[]']);
  return (function($base, $super) {
    function $Dir(){};
    var self = $Dir = $klass($base, $super, 'Dir', $Dir);

    var def = self.$$proto, $scope = self.$$scope;

    return (function(self) {
      var $scope = self.$$scope, def = self.$$proto;

      self.$$proto.$chdir = TMP_1 = function(dir) {
        var $a, self = this, $iter = TMP_1.$$p, $yield = $iter || nil, prev_cwd = nil;

        TMP_1.$$p = null;
        try {
        prev_cwd = Opal.current_dir;
        Opal.current_dir = dir;
        return $a = Opal.yieldX($yield, []), $a === $breaker ? $a : $a;
        } finally {
        Opal.current_dir = prev_cwd;
        };
      };
      self.$$proto.$pwd = function() {
        var self = this;

        return Opal.current_dir || '.';
      };
      self.$$proto.$getwd = self.$$proto.$pwd;
      return (self.$$proto.$home = function() {
        var $a, self = this;

        return ((($a = $scope.get('ENV')['$[]']("HOME")) !== false && $a !== nil) ? $a : ".");
      }, nil) && 'home';
    })(self.$singleton_class())
  })(self, null)
};

/* Generated by Opal 0.7.2 */
Opal.modules["corelib/file"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass, $range = Opal.range;

  Opal.add_stubs(['$join', '$compact', '$split', '$==', '$first', '$[]=', '$home', '$each', '$pop', '$<<', '$[]', '$gsub', '$find', '$=~']);
  return (function($base, $super) {
    function $File(){};
    var self = $File = $klass($base, $super, 'File', $File);

    var def = self.$$proto, $scope = self.$$scope;

    Opal.cdecl($scope, 'Separator', Opal.cdecl($scope, 'SEPARATOR', "/"));

    Opal.cdecl($scope, 'ALT_SEPARATOR', nil);

    Opal.cdecl($scope, 'PATH_SEPARATOR', ":");

    return (function(self) {
      var $scope = self.$$scope, def = self.$$proto;

      self.$$proto.$expand_path = function(path, basedir) {
        var $a, $b, TMP_1, self = this, parts = nil, new_parts = nil;

        if (basedir == null) {
          basedir = nil
        }
        path = [basedir, path].$compact().$join($scope.get('SEPARATOR'));
        parts = path.$split($scope.get('SEPARATOR'));
        new_parts = [];
        if (parts.$first()['$==']("~")) {
          parts['$[]='](0, $scope.get('Dir').$home())};
        ($a = ($b = parts).$each, $a.$$p = (TMP_1 = function(part){var self = TMP_1.$$s || this;
if (part == null) part = nil;
        if (part['$==']("..")) {
            return new_parts.$pop()
            } else {
            return new_parts['$<<'](part)
          }}, TMP_1.$$s = self, TMP_1), $a).call($b);
        return new_parts.$join($scope.get('SEPARATOR'));
      };
      self.$$proto.$dirname = function(path) {
        var self = this;

        return self.$split(path)['$[]']($range(0, -2, false));
      };
      self.$$proto.$basename = function(path) {
        var self = this;

        return self.$split(path)['$[]'](-1);
      };
      self.$$proto['$exist?'] = function(path) {
        var self = this;

        return Opal.modules[path] != null;
      };
      self.$$proto['$exists?'] = self.$$proto['$exist?'];
      self.$$proto['$directory?'] = function(path) {
        var $a, $b, TMP_2, self = this, files = nil, file = nil;

        files = [];
        
        for (var key in Opal.modules) {
          files.push(key)
        }
      ;
        path = path.$gsub((new RegExp("(^." + $scope.get('SEPARATOR') + "+|" + $scope.get('SEPARATOR') + "+$)")));
        file = ($a = ($b = files).$find, $a.$$p = (TMP_2 = function(file){var self = TMP_2.$$s || this;
if (file == null) file = nil;
        return file['$=~']((new RegExp("^" + path)))}, TMP_2.$$s = self, TMP_2), $a).call($b);
        return file;
      };
      self.$$proto.$join = function(paths) {
        var self = this;

        paths = $slice.call(arguments, 0);
        return paths.$join($scope.get('SEPARATOR')).$gsub((new RegExp("" + $scope.get('SEPARATOR') + "+")), $scope.get('SEPARATOR'));
      };
      return (self.$$proto.$split = function(path) {
        var self = this;

        return path.$split($scope.get('SEPARATOR'));
      }, nil) && 'split';
    })(self.$singleton_class());
  })(self, $scope.get('IO'))
};

/* Generated by Opal 0.7.2 */
Opal.modules["opal"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice;

  Opal.add_stubs(['$require']);
  self.$require("corelib/runtime");
  self.$require("corelib/helpers");
  self.$require("corelib/module");
  self.$require("corelib/class");
  self.$require("corelib/basic_object");
  self.$require("corelib/kernel");
  self.$require("corelib/nil_class");
  self.$require("corelib/boolean");
  self.$require("corelib/error");
  self.$require("corelib/regexp");
  self.$require("corelib/comparable");
  self.$require("corelib/enumerable");
  self.$require("corelib/enumerator");
  self.$require("corelib/array");
  self.$require("corelib/array/inheritance");
  self.$require("corelib/hash");
  self.$require("corelib/string");
  self.$require("corelib/string/inheritance");
  self.$require("corelib/match_data");
  self.$require("corelib/numeric");
  self.$require("corelib/complex");
  self.$require("corelib/rational");
  self.$require("corelib/proc");
  self.$require("corelib/method");
  self.$require("corelib/range");
  self.$require("corelib/time");
  self.$require("corelib/struct");
  self.$require("corelib/io");
  self.$require("corelib/main");
  self.$require("corelib/variables");
  self.$require("corelib/dir");
  return self.$require("corelib/file");
};

/* Generated by Opal 0.7.2 */
Opal.modules["native"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $range = Opal.range, $hash2 = Opal.hash2, $klass = Opal.klass, $gvars = Opal.gvars;

  Opal.add_stubs(['$try_convert', '$native?', '$respond_to?', '$to_n', '$raise', '$inspect', '$Native', '$end_with?', '$define_method', '$[]', '$convert', '$call', '$to_proc', '$new', '$each', '$native_reader', '$native_writer', '$extend', '$to_a', '$to_ary', '$include', '$method_missing', '$bind', '$instance_method', '$[]=', '$slice', '$-', '$length', '$enum_for', '$===', '$>=', '$<<', '$==', '$instance_variable_set', '$members', '$each_with_index', '$each_pair', '$name', '$native_module']);
  (function($base) {
    var self = $module($base, 'Native');

    var def = self.$$proto, $scope = self.$$scope, TMP_1;

    Opal.defs(self, '$is_a?', function(object, klass) {
      var self = this;

      
      try {
        return object instanceof self.$try_convert(klass);
      }
      catch (e) {
        return false;
      }
    ;
    });

    Opal.defs(self, '$try_convert', function(value) {
      var self = this;

      
      if (self['$native?'](value)) {
        return value;
      }
      else if (value['$respond_to?']("to_n")) {
        return value.$to_n();
      }
      else {
        return nil;
      }
    ;
    });

    Opal.defs(self, '$convert', function(value) {
      var self = this;

      
      if (self['$native?'](value)) {
        return value;
      }
      else if (value['$respond_to?']("to_n")) {
        return value.$to_n();
      }
      else {
        self.$raise($scope.get('ArgumentError'), "" + (value.$inspect()) + " isn't native");
      }
    ;
    });

    Opal.defs(self, '$call', TMP_1 = function(obj, key, args) {
      var self = this, $iter = TMP_1.$$p, block = $iter || nil;

      args = $slice.call(arguments, 2);
      TMP_1.$$p = null;
      
      var prop = obj[key];

      if (prop instanceof Function) {
        var converted = new Array(args.length);

        for (var i = 0, length = args.length; i < length; i++) {
          var item = args[i],
              conv = self.$try_convert(item);

          converted[i] = conv === nil ? item : conv;
        }

        if (block !== nil) {
          converted.push(block);
        }

        return self.$Native(prop.apply(obj, converted));
      }
      else {
        return self.$Native(prop);
      }
    ;
    });

    (function($base) {
      var self = $module($base, 'Helpers');

      var def = self.$$proto, $scope = self.$$scope;

      Opal.defn(self, '$alias_native', function(new$, old, options) {
        var $a, $b, TMP_2, $c, TMP_3, $d, TMP_4, self = this, as = nil;

        if (old == null) {
          old = new$
        }
        if (options == null) {
          options = $hash2([], {})
        }
        if ((($a = old['$end_with?']("=")) !== nil && (!$a.$$is_boolean || $a == true))) {
          return ($a = ($b = self).$define_method, $a.$$p = (TMP_2 = function(value){var self = TMP_2.$$s || this;
            if (self["native"] == null) self["native"] = nil;
if (value == null) value = nil;
          self["native"][old['$[]']($range(0, -2, false))] = $scope.get('Native').$convert(value);
            return value;}, TMP_2.$$s = self, TMP_2), $a).call($b, new$)
        } else if ((($a = as = options['$[]']("as")) !== nil && (!$a.$$is_boolean || $a == true))) {
          return ($a = ($c = self).$define_method, $a.$$p = (TMP_3 = function(args){var self = TMP_3.$$s || this, block, $a, $b, $c;
            if (self["native"] == null) self["native"] = nil;
args = $slice.call(arguments, 0);
            block = TMP_3.$$p || nil, TMP_3.$$p = null;
          if ((($a = value = ($b = ($c = $scope.get('Native')).$call, $b.$$p = block.$to_proc(), $b).apply($c, [self["native"], old].concat(args))) !== nil && (!$a.$$is_boolean || $a == true))) {
              return as.$new(value.$to_n())
              } else {
              return nil
            }}, TMP_3.$$s = self, TMP_3), $a).call($c, new$)
          } else {
          return ($a = ($d = self).$define_method, $a.$$p = (TMP_4 = function(args){var self = TMP_4.$$s || this, block, $a, $b;
            if (self["native"] == null) self["native"] = nil;
args = $slice.call(arguments, 0);
            block = TMP_4.$$p || nil, TMP_4.$$p = null;
          return ($a = ($b = $scope.get('Native')).$call, $a.$$p = block.$to_proc(), $a).apply($b, [self["native"], old].concat(args))}, TMP_4.$$s = self, TMP_4), $a).call($d, new$)
        };
      });

      Opal.defn(self, '$native_reader', function(names) {
        var $a, $b, TMP_5, self = this;

        names = $slice.call(arguments, 0);
        return ($a = ($b = names).$each, $a.$$p = (TMP_5 = function(name){var self = TMP_5.$$s || this, $a, $b, TMP_6;
if (name == null) name = nil;
        return ($a = ($b = self).$define_method, $a.$$p = (TMP_6 = function(){var self = TMP_6.$$s || this;
            if (self["native"] == null) self["native"] = nil;

          return self.$Native(self["native"][name])}, TMP_6.$$s = self, TMP_6), $a).call($b, name)}, TMP_5.$$s = self, TMP_5), $a).call($b);
      });

      Opal.defn(self, '$native_writer', function(names) {
        var $a, $b, TMP_7, self = this;

        names = $slice.call(arguments, 0);
        return ($a = ($b = names).$each, $a.$$p = (TMP_7 = function(name){var self = TMP_7.$$s || this, $a, $b, TMP_8;
if (name == null) name = nil;
        return ($a = ($b = self).$define_method, $a.$$p = (TMP_8 = function(value){var self = TMP_8.$$s || this;
            if (self["native"] == null) self["native"] = nil;
if (value == null) value = nil;
          return self.$Native(self["native"][name] = value)}, TMP_8.$$s = self, TMP_8), $a).call($b, "" + (name) + "=")}, TMP_7.$$s = self, TMP_7), $a).call($b);
      });

      Opal.defn(self, '$native_accessor', function(names) {
        var $a, $b, self = this;

        names = $slice.call(arguments, 0);
        ($a = self).$native_reader.apply($a, [].concat(names));
        return ($b = self).$native_writer.apply($b, [].concat(names));
      });
    })(self);

    Opal.defs(self, '$included', function(klass) {
      var self = this;

      return klass.$extend($scope.get('Helpers'));
    });

    Opal.defn(self, '$initialize', function(native$) {
      var $a, self = this;

      if ((($a = $scope.get('Kernel')['$native?'](native$)) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        $scope.get('Kernel').$raise($scope.get('ArgumentError'), "" + (native$.$inspect()) + " isn't native")
      };
      return self["native"] = native$;
    });

    Opal.defn(self, '$to_n', function() {
      var self = this;
      if (self["native"] == null) self["native"] = nil;

      return self["native"];
    });
  })(self);
  (function($base) {
    var self = $module($base, 'Kernel');

    var def = self.$$proto, $scope = self.$$scope, TMP_9;

    Opal.defn(self, '$native?', function(value) {
      var self = this;

      return value == null || !value.$$class;
    });

    Opal.defn(self, '$Native', function(obj) {
      var $a, self = this;

      if ((($a = obj == null) !== nil && (!$a.$$is_boolean || $a == true))) {
        return nil
      } else if ((($a = self['$native?'](obj)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return (($scope.get('Native')).$$scope.get('Object')).$new(obj)
        } else {
        return obj
      };
    });

    Opal.defn(self, '$Array', TMP_9 = function(object, args) {
      var $a, $b, self = this, $iter = TMP_9.$$p, block = $iter || nil;

      args = $slice.call(arguments, 1);
      TMP_9.$$p = null;
      
      if (object == null || object === nil) {
        return [];
      }
      else if (self['$native?'](object)) {
        return ($a = ($b = (($scope.get('Native')).$$scope.get('Array'))).$new, $a.$$p = block.$to_proc(), $a).apply($b, [object].concat(args)).$to_a();
      }
      else if (object['$respond_to?']("to_ary")) {
        return object.$to_ary();
      }
      else if (object['$respond_to?']("to_a")) {
        return object.$to_a();
      }
      else {
        return [object];
      }
    ;
    });
  })(self);
  (function($base, $super) {
    function $Object(){};
    var self = $Object = $klass($base, $super, 'Object', $Object);

    var def = self.$$proto, $scope = self.$$scope, TMP_10, TMP_11, TMP_12;

    def["native"] = nil;
    self.$include(Opal.get('Native'));

    Opal.defn(self, '$==', function(other) {
      var self = this;

      return self["native"] === $scope.get('Native').$try_convert(other);
    });

    Opal.defn(self, '$has_key?', function(name) {
      var self = this;

      return Opal.hasOwnProperty.call(self["native"], name);
    });

    Opal.defn(self, '$key?', def['$has_key?']);

    Opal.defn(self, '$include?', def['$has_key?']);

    Opal.defn(self, '$member?', def['$has_key?']);

    Opal.defn(self, '$each', TMP_10 = function(args) {
      var $a, self = this, $iter = TMP_10.$$p, $yield = $iter || nil;

      args = $slice.call(arguments, 0);
      TMP_10.$$p = null;
      if (($yield !== nil)) {
        
        for (var key in self["native"]) {
          ((($a = Opal.yieldX($yield, [key, self["native"][key]])) === $breaker) ? $breaker.$v : $a)
        }
      ;
        return self;
        } else {
        return ($a = self).$method_missing.apply($a, ["each"].concat(args))
      };
    });

    Opal.defn(self, '$[]', function(key) {
      var self = this;

      
      var prop = self["native"][key];

      if (prop instanceof Function) {
        return prop;
      }
      else {
        return Opal.get('Native').$call(self["native"], key)
      }
    ;
    });

    Opal.defn(self, '$[]=', function(key, value) {
      var $a, self = this, native$ = nil;

      native$ = $scope.get('Native').$try_convert(value);
      if ((($a = native$ === nil) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self["native"][key] = value;
        } else {
        return self["native"][key] = native$;
      };
    });

    Opal.defn(self, '$merge!', function(other) {
      var self = this;

      
      var other = $scope.get('Native').$convert(other);

      for (var prop in other) {
        self["native"][prop] = other[prop];
      }
    ;
      return self;
    });

    Opal.defn(self, '$respond_to?', function(name, include_all) {
      var self = this;

      if (include_all == null) {
        include_all = false
      }
      return $scope.get('Kernel').$instance_method("respond_to?").$bind(self).$call(name, include_all);
    });

    Opal.defn(self, '$respond_to_missing?', function(name) {
      var self = this;

      return Opal.hasOwnProperty.call(self["native"], name);
    });

    Opal.defn(self, '$method_missing', TMP_11 = function(mid, args) {
      var $a, $b, self = this, $iter = TMP_11.$$p, block = $iter || nil;

      args = $slice.call(arguments, 1);
      TMP_11.$$p = null;
      
      if (mid.charAt(mid.length - 1) === '=') {
        return self['$[]='](mid.$slice(0, mid.$length()['$-'](1)), args['$[]'](0));
      }
      else {
        return ($a = ($b = Opal.get('Native')).$call, $a.$$p = block.$to_proc(), $a).apply($b, [self["native"], mid].concat(args));
      }
    ;
    });

    Opal.defn(self, '$nil?', function() {
      var self = this;

      return false;
    });

    Opal.defn(self, '$is_a?', function(klass) {
      var self = this;

      return Opal.is_a(self, klass);
    });

    Opal.defn(self, '$kind_of?', def['$is_a?']);

    Opal.defn(self, '$instance_of?', function(klass) {
      var self = this;

      return self.$$class === klass;
    });

    Opal.defn(self, '$class', function() {
      var self = this;

      return self.$$class;
    });

    Opal.defn(self, '$to_a', TMP_12 = function(options) {
      var $a, $b, self = this, $iter = TMP_12.$$p, block = $iter || nil;

      if (options == null) {
        options = $hash2([], {})
      }
      TMP_12.$$p = null;
      return ($a = ($b = (($scope.get('Native')).$$scope.get('Array'))).$new, $a.$$p = block.$to_proc(), $a).call($b, self["native"], options).$to_a();
    });

    return (Opal.defn(self, '$inspect', function() {
      var self = this;

      return "#<Native:" + (String(self["native"])) + ">";
    }), nil) && 'inspect';
  })($scope.get('Native'), $scope.get('BasicObject'));
  (function($base, $super) {
    function $Array(){};
    var self = $Array = $klass($base, $super, 'Array', $Array);

    var def = self.$$proto, $scope = self.$$scope, TMP_13, TMP_14;

    def.named = def["native"] = def.get = def.block = def.set = def.length = nil;
    self.$include($scope.get('Native'));

    self.$include($scope.get('Enumerable'));

    def.$initialize = TMP_13 = function(native$, options) {
      var $a, self = this, $iter = TMP_13.$$p, block = $iter || nil;

      if (options == null) {
        options = $hash2([], {})
      }
      TMP_13.$$p = null;
      Opal.find_super_dispatcher(self, 'initialize', TMP_13, null).apply(self, [native$]);
      self.get = ((($a = options['$[]']("get")) !== false && $a !== nil) ? $a : options['$[]']("access"));
      self.named = options['$[]']("named");
      self.set = ((($a = options['$[]']("set")) !== false && $a !== nil) ? $a : options['$[]']("access"));
      self.length = ((($a = options['$[]']("length")) !== false && $a !== nil) ? $a : "length");
      self.block = block;
      if ((($a = self.$length() == null) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self.$raise($scope.get('ArgumentError'), "no length found on the array-like object")
        } else {
        return nil
      };
    };

    def.$each = TMP_14 = function() {
      var self = this, $iter = TMP_14.$$p, block = $iter || nil;

      TMP_14.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        return self.$enum_for("each")
      };
      
      for (var i = 0, length = self.$length(); i < length; i++) {
        var value = Opal.yield1(block, self['$[]'](i));

        if (value === $breaker) {
          return $breaker.$v;
        }
      }
    ;
      return self;
    };

    def['$[]'] = function(index) {
      var $a, self = this, result = nil, $case = nil;

      result = (function() {$case = index;if ($scope.get('String')['$===']($case) || $scope.get('Symbol')['$===']($case)) {if ((($a = self.named) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self["native"][self.named](index);
        } else {
        return self["native"][index];
      }}else if ($scope.get('Integer')['$===']($case)) {if ((($a = self.get) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self["native"][self.get](index);
        } else {
        return self["native"][index];
      }}else { return nil }})();
      if (result !== false && result !== nil) {
        if ((($a = self.block) !== nil && (!$a.$$is_boolean || $a == true))) {
          return self.block.$call(result)
          } else {
          return self.$Native(result)
        }
        } else {
        return nil
      };
    };

    def['$[]='] = function(index, value) {
      var $a, self = this;

      if ((($a = self.set) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self["native"][self.set](index, $scope.get('Native').$convert(value));
        } else {
        return self["native"][index] = $scope.get('Native').$convert(value);
      };
    };

    def.$last = function(count) {
      var $a, self = this, index = nil, result = nil;

      if (count == null) {
        count = nil
      }
      if (count !== false && count !== nil) {
        index = self.$length()['$-'](1);
        result = [];
        while (index['$>='](0)) {
        result['$<<'](self['$[]'](index));
        index = index['$-'](1);};
        return result;
        } else {
        return self['$[]'](self.$length()['$-'](1))
      };
    };

    def.$length = function() {
      var self = this;

      return self["native"][self.length];
    };

    Opal.defn(self, '$to_ary', def.$to_a);

    return (def.$inspect = function() {
      var self = this;

      return self.$to_a().$inspect();
    }, nil) && 'inspect';
  })($scope.get('Native'), null);
  (function($base, $super) {
    function $Numeric(){};
    var self = $Numeric = $klass($base, $super, 'Numeric', $Numeric);

    var def = self.$$proto, $scope = self.$$scope;

    return (def.$to_n = function() {
      var self = this;

      return self.valueOf();
    }, nil) && 'to_n'
  })(self, null);
  (function($base, $super) {
    function $Proc(){};
    var self = $Proc = $klass($base, $super, 'Proc', $Proc);

    var def = self.$$proto, $scope = self.$$scope;

    return (def.$to_n = function() {
      var self = this;

      return self;
    }, nil) && 'to_n'
  })(self, null);
  (function($base, $super) {
    function $String(){};
    var self = $String = $klass($base, $super, 'String', $String);

    var def = self.$$proto, $scope = self.$$scope;

    return (def.$to_n = function() {
      var self = this;

      return self.valueOf();
    }, nil) && 'to_n'
  })(self, null);
  (function($base, $super) {
    function $Regexp(){};
    var self = $Regexp = $klass($base, $super, 'Regexp', $Regexp);

    var def = self.$$proto, $scope = self.$$scope;

    return (def.$to_n = function() {
      var self = this;

      return self.valueOf();
    }, nil) && 'to_n'
  })(self, null);
  (function($base, $super) {
    function $MatchData(){};
    var self = $MatchData = $klass($base, $super, 'MatchData', $MatchData);

    var def = self.$$proto, $scope = self.$$scope;

    def.matches = nil;
    return (def.$to_n = function() {
      var self = this;

      return self.matches;
    }, nil) && 'to_n'
  })(self, null);
  (function($base, $super) {
    function $Struct(){};
    var self = $Struct = $klass($base, $super, 'Struct', $Struct);

    var def = self.$$proto, $scope = self.$$scope;

    def.$initialize = function(args) {
      var $a, $b, TMP_15, $c, TMP_16, self = this, object = nil;

      args = $slice.call(arguments, 0);
      if ((($a = (($b = args.$length()['$=='](1)) ? self['$native?'](args['$[]'](0)) : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
        object = args['$[]'](0);
        return ($a = ($b = self.$members()).$each, $a.$$p = (TMP_15 = function(name){var self = TMP_15.$$s || this;
if (name == null) name = nil;
        return self.$instance_variable_set("@" + (name), self.$Native(object[name]))}, TMP_15.$$s = self, TMP_15), $a).call($b);
        } else {
        return ($a = ($c = self.$members()).$each_with_index, $a.$$p = (TMP_16 = function(name, index){var self = TMP_16.$$s || this;
if (name == null) name = nil;if (index == null) index = nil;
        return self.$instance_variable_set("@" + (name), args['$[]'](index))}, TMP_16.$$s = self, TMP_16), $a).call($c)
      };
    };

    return (def.$to_n = function() {
      var $a, $b, TMP_17, self = this, result = nil;

      result = {};
      ($a = ($b = self).$each_pair, $a.$$p = (TMP_17 = function(name, value){var self = TMP_17.$$s || this;
if (name == null) name = nil;if (value == null) value = nil;
      return result[name] = value.$to_n();}, TMP_17.$$s = self, TMP_17), $a).call($b);
      return result;
    }, nil) && 'to_n';
  })(self, null);
  (function($base, $super) {
    function $Array(){};
    var self = $Array = $klass($base, $super, 'Array', $Array);

    var def = self.$$proto, $scope = self.$$scope;

    return (def.$to_n = function() {
      var self = this;

      
      var result = [];

      for (var i = 0, length = self.length; i < length; i++) {
        var obj = self[i];

        if ((obj)['$respond_to?']("to_n")) {
          result.push((obj).$to_n());
        }
        else {
          result.push(obj);
        }
      }

      return result;
    ;
    }, nil) && 'to_n'
  })(self, null);
  (function($base, $super) {
    function $Boolean(){};
    var self = $Boolean = $klass($base, $super, 'Boolean', $Boolean);

    var def = self.$$proto, $scope = self.$$scope;

    return (def.$to_n = function() {
      var self = this;

      return self.valueOf();
    }, nil) && 'to_n'
  })(self, null);
  (function($base, $super) {
    function $Time(){};
    var self = $Time = $klass($base, $super, 'Time', $Time);

    var def = self.$$proto, $scope = self.$$scope;

    return (def.$to_n = function() {
      var self = this;

      return self;
    }, nil) && 'to_n'
  })(self, null);
  (function($base, $super) {
    function $NilClass(){};
    var self = $NilClass = $klass($base, $super, 'NilClass', $NilClass);

    var def = self.$$proto, $scope = self.$$scope;

    return (def.$to_n = function() {
      var self = this;

      return null;
    }, nil) && 'to_n'
  })(self, null);
  (function($base, $super) {
    function $Hash(){};
    var self = $Hash = $klass($base, $super, 'Hash', $Hash);

    var def = self.$$proto, $scope = self.$$scope, TMP_18;

    def.$initialize = TMP_18 = function(defaults) {
      var self = this, $iter = TMP_18.$$p, block = $iter || nil;

      TMP_18.$$p = null;
      
      if (defaults != null) {
        if (defaults.constructor === Object) {
          var _map = self.map,
              smap = self.smap,
              keys = self.keys,
              map, khash, value;

          for (var key in defaults) {
            value = defaults[key];

            if (key.$$is_string) {
              map = smap;
              khash = key;
            } else {
              map = _map;
              khash = key.$hash();
            }

            if (value && value.constructor === Object) {
              map[khash] = $scope.get('Hash').$new(value);
            }
            else {
              map[khash] = self.$Native(value);
            }

            keys.push(key);
          }
        }
        else {
          self.none = defaults;
        }
      }
      else if (block !== nil) {
        self.proc = block;
      }

      return self;
    
    };

    return (def.$to_n = function() {
      var self = this;

      
      var result = {},
          keys   = self.keys,
          _map   = self.map,
          smap   = self.smap,
          map, khash, value;

      for (var i = 0, length = keys.length; i < length; i++) {
        key   = keys[i];

        if (key.$$is_string) {
          map = smap;
          khash = key;
        } else {
          map = _map;
          khash = key.$hash();
        }

        value = map[khash];

        if ((value)['$respond_to?']("to_n")) {
          result[key] = (value).$to_n();
        }
        else {
          result[key] = value;
        }
      }

      return result;
    ;
    }, nil) && 'to_n';
  })(self, null);
  (function($base, $super) {
    function $Module(){};
    var self = $Module = $klass($base, $super, 'Module', $Module);

    var def = self.$$proto, $scope = self.$$scope;

    return (def.$native_module = function() {
      var self = this;

      return Opal.global[self.$name()] = self;
    }, nil) && 'native_module'
  })(self, null);
  (function($base, $super) {
    function $Class(){};
    var self = $Class = $klass($base, $super, 'Class', $Class);

    var def = self.$$proto, $scope = self.$$scope;

    def.$native_alias = function(new_jsid, existing_mid) {
      var self = this;

      
      var aliased = self.$$proto['$' + existing_mid];
      if (!aliased) {
        self.$raise($scope.get('NameError'), "undefined method `" + (existing_mid) + "' for class `" + (self.$inspect()) + "'");
      }
      self.$$proto[new_jsid] = aliased;
    ;
    };

    return (def.$native_class = function() {
      var self = this;

      self.$native_module();
      self.new = self.$new;
    }, nil) && 'native_class';
  })(self, null);
  return $gvars.$ = $gvars.global = self.$Native(Opal.global);
};

/* Generated by Opal 0.7.2 */
Opal.modules["opal/jquery/constants"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var $a, self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $gvars = Opal.gvars;
  if ($gvars.$ == null) $gvars.$ = nil;

  Opal.add_stubs(['$require', '$[]', '$raise']);
  self.$require("native");
  if ((($a = ($scope.JQUERY_CLASS != null)) !== nil && (!$a.$$is_boolean || $a == true))) {
    return nil
    } else {
    return (function() {if ((($a = $gvars.$['$[]']("jQuery")) !== nil && (!$a.$$is_boolean || $a == true))) {return Opal.cdecl($scope, 'JQUERY_CLASS', Opal.cdecl($scope, 'JQUERY_SELECTOR', $gvars.$['$[]']("jQuery")))}else if ((($a = $gvars.$['$[]']("Zepto")) !== nil && (!$a.$$is_boolean || $a == true))) {Opal.cdecl($scope, 'JQUERY_SELECTOR', $gvars.$['$[]']("Zepto"));
    return Opal.cdecl($scope, 'JQUERY_CLASS', $gvars.$['$[]']("Zepto")['$[]']("zepto")['$[]']("Z"));}else {return self.$raise($scope.get('NameError'), "Can't find jQuery or Zepto. jQuery must be included before opal-jquery")}})()
  };
};

/* Generated by Opal 0.7.2 */
Opal.modules["opal/jquery/element"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  Opal.add_stubs(['$require', '$to_n', '$include', '$each', '$alias_native', '$attr_reader', '$nil?', '$is_a?', '$has_key?', '$delete', '$call', '$gsub', '$upcase', '$[]', '$compact', '$map', '$respond_to?', '$<<', '$Native', '$new']);
  self.$require("native");
  self.$require("opal/jquery/constants");
  return (function($base, $super) {
    function $Element(){};
    var self = $Element = $klass($base, $super, 'Element', $Element);

    var def = self.$$proto, $scope = self.$$scope, TMP_2, TMP_3, TMP_6, TMP_7;

    var $ = $scope.get('JQUERY_SELECTOR').$to_n();

    self.$include($scope.get('Enumerable'));

    Opal.defs(self, '$find', function(selector) {
      var self = this;

      return $(selector);
    });

    Opal.defs(self, '$[]', function(selector) {
      var self = this;

      return $(selector);
    });

    Opal.defs(self, '$id', function(id) {
      var self = this;

      
      var el = document.getElementById(id);

      if (!el) {
        return nil;
      }

      return $(el);
    
    });

    Opal.defs(self, '$new', function(tag) {
      var self = this;

      if (tag == null) {
        tag = "div"
      }
      return $(document.createElement(tag));
    });

    Opal.defs(self, '$parse', function(str) {
      var self = this;

      return $(str);
    });

    Opal.defs(self, '$expose', function(methods) {
      var $a, $b, TMP_1, self = this;

      methods = $slice.call(arguments, 0);
      return ($a = ($b = methods).$each, $a.$$p = (TMP_1 = function(method){var self = TMP_1.$$s || this;
if (method == null) method = nil;
      return self.$alias_native(method)}, TMP_1.$$s = self, TMP_1), $a).call($b);
    });

    self.$attr_reader("selector");

    self.$alias_native("after");

    self.$alias_native("before");

    self.$alias_native("parent");

    self.$alias_native("parents");

    self.$alias_native("prev");

    self.$alias_native("remove");

    self.$alias_native("hide");

    self.$alias_native("show");

    self.$alias_native("toggle");

    self.$alias_native("children");

    self.$alias_native("blur");

    self.$alias_native("closest");

    self.$alias_native("detach");

    self.$alias_native("focus");

    self.$alias_native("find");

    self.$alias_native("next");

    self.$alias_native("siblings");

    self.$alias_native("text");

    self.$alias_native("trigger");

    self.$alias_native("append");

    self.$alias_native("serialize");

    self.$alias_native("is");

    self.$alias_native("filter");

    self.$alias_native("last");

    self.$alias_native("wrap");

    self.$alias_native("stop");

    self.$alias_native("clone");

    self.$alias_native("empty");

    self.$alias_native("get");

    self.$alias_native("prop");

    Opal.defn(self, '$succ', def.$next);

    Opal.defn(self, '$<<', def.$append);

    self.$alias_native("[]=", "attr");

    self.$alias_native("add_class", "addClass");

    self.$alias_native("append_to", "appendTo");

    self.$alias_native("has_class?", "hasClass");

    self.$alias_native("html=", "html");

    self.$alias_native("remove_attr", "removeAttr");

    self.$alias_native("remove_class", "removeClass");

    self.$alias_native("text=", "text");

    self.$alias_native("toggle_class", "toggleClass");

    self.$alias_native("value=", "val");

    self.$alias_native("scroll_top=", "scrollTop");

    self.$alias_native("scroll_top", "scrollTop");

    self.$alias_native("scroll_left=", "scrollLeft");

    self.$alias_native("scroll_left", "scrollLeft");

    self.$alias_native("remove_attribute", "removeAttr");

    self.$alias_native("slide_down", "slideDown");

    self.$alias_native("slide_up", "slideUp");

    self.$alias_native("slide_toggle", "slideToggle");

    self.$alias_native("fade_toggle", "fadeToggle");

    self.$alias_native("height=", "height");

    self.$alias_native("width=", "width");

    self.$alias_native("outer_width", "outerWidth");

    self.$alias_native("outer_height", "outerHeight");

    def.$to_n = function() {
      var self = this;

      return self;
    };

    def['$[]'] = function(name) {
      var self = this;

      return self.attr(name) || nil;
    };

    def.$attr = function(name, value) {
      var $a, self = this;

      if (value == null) {
        value = nil
      }
      if ((($a = value['$nil?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self.attr(name) || nil;
        } else {
        return self.attr(name, value);
      };
    };

    def['$has_attribute?'] = function(name) {
      var self = this;

      return !!self.attr(name);
    };

    def.$append_to_body = function() {
      var self = this;

      return self.appendTo(document.body);
    };

    def.$append_to_head = function() {
      var self = this;

      return self.appendTo(document.head);
    };

    def.$at = function(index) {
      var self = this;

      
      var length = self.length;

      if (index < 0) {
        index += length;
      }

      if (index < 0 || index >= length) {
        return nil;
      }

      return $(self[index]);
    
    };

    def.$class_name = function() {
      var self = this;

      
      var first = self[0];
      return (first && first.className) || "";
    
    };

    def['$class_name='] = function(name) {
      var self = this;

      
      for (var i = 0, length = self.length; i < length; i++) {
        self[i].className = name;
      }
    
      return self;
    };

    def.$css = function(name, value) {
      var $a, $b, self = this;

      if (value == null) {
        value = nil
      }
      if ((($a = ($b = value['$nil?'](), $b !== false && $b !== nil ?name['$is_a?']($scope.get('String')) : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self.css(name)
      } else if ((($a = name['$is_a?']($scope.get('Hash'))) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.css(name.$to_n());
        } else {
        self.css(name, value);
      };
      return self;
    };

    def.$animate = TMP_2 = function(params) {
      var $a, self = this, $iter = TMP_2.$$p, block = $iter || nil, speed = nil;

      TMP_2.$$p = null;
      speed = (function() {if ((($a = params['$has_key?']("speed")) !== nil && (!$a.$$is_boolean || $a == true))) {
        return params.$delete("speed")
        } else {
        return 400
      }; return nil; })();
      
      self.animate(params.$to_n(), speed, function() {
        (function() {if ((block !== nil)) {
        return block.$call()
        } else {
        return nil
      }; return nil; })()
      })
    ;
    };

    def.$data = function(args) {
      var self = this;

      args = $slice.call(arguments, 0);
      
      var result = self.data.apply(self, args);
      return result == null ? nil : result;
    
    };

    def.$effect = TMP_3 = function(name, args) {
      var $a, $b, TMP_4, $c, TMP_5, self = this, $iter = TMP_3.$$p, block = $iter || nil;

      args = $slice.call(arguments, 1);
      TMP_3.$$p = null;
      name = ($a = ($b = name).$gsub, $a.$$p = (TMP_4 = function(match){var self = TMP_4.$$s || this;
if (match == null) match = nil;
      return match['$[]'](1).$upcase()}, TMP_4.$$s = self, TMP_4), $a).call($b, /_\w/);
      args = ($a = ($c = args).$map, $a.$$p = (TMP_5 = function(a){var self = TMP_5.$$s || this, $a;
if (a == null) a = nil;
      if ((($a = a['$respond_to?']("to_n")) !== nil && (!$a.$$is_boolean || $a == true))) {
          return a.$to_n()
          } else {
          return nil
        }}, TMP_5.$$s = self, TMP_5), $a).call($c).$compact();
      args['$<<'](function() { (function() {if ((block !== nil)) {
        return block.$call()
        } else {
        return nil
      }; return nil; })() });
      return self[name].apply(self, args);
    };

    def['$visible?'] = function() {
      var self = this;

      return self.is(':visible');
    };

    def.$offset = function() {
      var self = this;

      return self.$Native(self.offset());
    };

    def.$each = TMP_6 = function() {
      var self = this, $iter = TMP_6.$$p, $yield = $iter || nil;

      TMP_6.$$p = null;
      for (var i = 0, length = self.length; i < length; i++) {
      if (Opal.yield1($yield, $(self[i])) === $breaker) return $breaker.$v;
      };
      return self;
    };

    def.$first = function() {
      var self = this;

      return self.length ? self.first() : nil;
    };

    def.$html = function(content) {
      var self = this;

      
      if (content != null) {
        return self.html(content);
      }

      return self.html() || '';
    
    };

    def.$id = function() {
      var self = this;

      
      var first = self[0];
      return (first && first.id) || "";
    
    };

    def['$id='] = function(id) {
      var self = this;

      
      var first = self[0];

      if (first) {
        first.id = id;
      }

      return self;
    
    };

    def.$tag_name = function() {
      var self = this;

      return self.length > 0 ? self[0].tagName.toLowerCase() : nil;
    };

    def.$inspect = function() {
      var self = this;

      
      if      (self[0] === document) return '#<Element [document]>'
      else if (self[0] === window  ) return '#<Element [window]>'

      var val, el, str, result = [];

      for (var i = 0, length = self.length; i < length; i++) {
        el  = self[i];
        if (!el.tagName) { return '#<Element ['+el.toString()+']'; }

        str = "<" + el.tagName.toLowerCase();

        if (val = el.id) str += (' id="' + val + '"');
        if (val = el.className) str += (' class="' + val + '"');

        result.push(str + '>');
      }

      return '#<Element [' + result.join(', ') + ']>';
    
    };

    def.$to_s = function() {
      var self = this;

      
      var val, el, result = [];

      for (var i = 0, length = self.length; i < length; i++) {
        el  = self[i];

        result.push(el.outerHTML)
      }

      return result.join(', ');
    
    };

    def.$length = function() {
      var self = this;

      return self.length;
    };

    def['$any?'] = function() {
      var self = this;

      return self.length > 0;
    };

    def['$empty?'] = function() {
      var self = this;

      return self.length === 0;
    };

    Opal.defn(self, '$empty?', def['$none?']);

    def.$on = TMP_7 = function(name, sel) {
      var self = this, $iter = TMP_7.$$p, block = $iter || nil;

      if (sel == null) {
        sel = nil
      }
      TMP_7.$$p = null;
      
      var wrapper = function(evt) {
        if (evt.preventDefault) {
          evt = $scope.get('Event').$new(evt);
        }

        return block.apply(null, arguments);
      };

      block._jq_wrap = wrapper;

      if (sel == nil) {
        self.on(name, wrapper);
      }
      else {
        self.on(name, sel, wrapper);
      }
    ;
      return block;
    };

    def.$off = function(name, sel, block) {
      var self = this;

      if (block == null) {
        block = nil
      }
      
      if (sel == null) {
        return self.off(name);
      }
      else if (block === nil) {
        return self.off(name, sel._jq_wrap);
      }
      else {
        return self.off(name, sel, block._jq_wrap);
      }
    
    };

    Opal.defn(self, '$size', def.$length);

    def.$value = function() {
      var self = this;

      return self.val() || "";
    };

    def.$height = function() {
      var self = this;

      return self.height() || nil;
    };

    def.$width = function() {
      var self = this;

      return self.width() || nil;
    };

    return (def.$position = function() {
      var self = this;

      return self.$Native(self.position());
    }, nil) && 'position';
  })(self, $scope.get('JQUERY_CLASS').$to_n());
};

/* Generated by Opal 0.7.2 */
Opal.modules["opal/jquery/window"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass, $gvars = Opal.gvars;

  Opal.add_stubs(['$require', '$include', '$find', '$on', '$to_proc', '$element', '$off', '$trigger', '$new']);
  self.$require("opal/jquery/element");
  (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base, $super) {
      function $Window(){};
      var self = $Window = $klass($base, $super, 'Window', $Window);

      var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_2;

      def.element = nil;
      self.$include($scope.get('Native'));

      def.$element = function() {
        var $a, self = this;

        return ((($a = self.element) !== false && $a !== nil) ? $a : self.element = $scope.get('Element').$find(window));
      };

      def.$on = TMP_1 = function(args) {
        var $a, $b, self = this, $iter = TMP_1.$$p, block = $iter || nil;

        args = $slice.call(arguments, 0);
        TMP_1.$$p = null;
        return ($a = ($b = self.$element()).$on, $a.$$p = block.$to_proc(), $a).apply($b, [].concat(args));
      };

      def.$off = TMP_2 = function(args) {
        var $a, $b, self = this, $iter = TMP_2.$$p, block = $iter || nil;

        args = $slice.call(arguments, 0);
        TMP_2.$$p = null;
        return ($a = ($b = self.$element()).$off, $a.$$p = block.$to_proc(), $a).apply($b, [].concat(args));
      };

      return (def.$trigger = function(args) {
        var $a, self = this;

        args = $slice.call(arguments, 0);
        return ($a = self.$element()).$trigger.apply($a, [].concat(args));
      }, nil) && 'trigger';
    })(self, null)
  })(self);
  Opal.cdecl($scope, 'Window', (($scope.get('Browser')).$$scope.get('Window')).$new(window));
  return $gvars.window = $scope.get('Window');
};

/* Generated by Opal 0.7.2 */
Opal.modules["opal/jquery/document"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $gvars = Opal.gvars;

  Opal.add_stubs(['$require', '$to_n', '$find', '$send']);
  self.$require("opal/jquery/constants");
  self.$require("opal/jquery/element");
  (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DocumentMethods');

      var def = self.$$proto, $scope = self.$$scope, TMP_1;

      var $ = $scope.get('JQUERY_SELECTOR').$to_n();

      Opal.defn(self, '$ready?', TMP_1 = function() {
        var self = this, $iter = TMP_1.$$p, block = $iter || nil;

        TMP_1.$$p = null;
        if ((block !== nil)) {
          return $(block);
          } else {
          return nil
        };
      });

      Opal.defn(self, '$title', function() {
        var self = this;

        return document.title;
      });

      Opal.defn(self, '$title=', function(title) {
        var self = this;

        return document.title = title;
      });

      Opal.defn(self, '$head', function() {
        var self = this;

        return $scope.get('Element').$find(document.head);
      });

      Opal.defn(self, '$body', function() {
        var self = this;

        return $scope.get('Element').$find(document.body);
      });
    })(self)
  })(self);
  Opal.cdecl($scope, 'Document', $scope.get('Element').$find(document));
  $scope.get('Document').$send("extend", (($scope.get('Browser')).$$scope.get('DocumentMethods')));
  return $gvars.document = $scope.get('Document');
};

/* Generated by Opal 0.7.2 */
Opal.modules["opal/jquery/event"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  Opal.add_stubs(['$require', '$to_n', '$stop', '$prevent']);
  self.$require("opal/jquery/constants");
  return (function($base, $super) {
    function $Event(){};
    var self = $Event = $klass($base, $super, 'Event', $Event);

    var def = self.$$proto, $scope = self.$$scope;

    def["native"] = nil;
    var $ = $scope.get('JQUERY_SELECTOR').$to_n();

    def.$initialize = function(native$) {
      var self = this;

      return self["native"] = native$;
    };

    def.$to_n = function() {
      var self = this;

      return self["native"];
    };

    def['$[]'] = function(name) {
      var self = this;

      return self["native"][name];
    };

    def.$type = function() {
      var self = this;

      return self["native"].type;
    };

    def.$element = function() {
      var self = this;

      return $(self["native"].currentTarget);
    };

    Opal.defn(self, '$current_target', def.$element);

    def.$target = function() {
      var self = this;

      return $(self["native"].target);
    };

    def['$prevented?'] = function() {
      var self = this;

      return self["native"].isDefaultPrevented();
    };

    def.$prevent = function() {
      var self = this;

      return self["native"].preventDefault();
    };

    def['$stopped?'] = function() {
      var self = this;

      return self["native"].isPropagationStopped();
    };

    def.$stop = function() {
      var self = this;

      return self["native"].stopPropagation();
    };

    def.$stop_immediate = function() {
      var self = this;

      return self["native"].stopImmediatePropagation();
    };

    def.$kill = function() {
      var self = this;

      self.$stop();
      return self.$prevent();
    };

    def.$page_x = function() {
      var self = this;

      return self["native"].pageX;
    };

    def.$page_y = function() {
      var self = this;

      return self["native"].pageY;
    };

    def.$touch_x = function() {
      var self = this;

      return self["native"].originalEvent.touches[0].pageX;
    };

    def.$touch_y = function() {
      var self = this;

      return self["native"].originalEvent.touches[0].pageY;
    };

    def.$ctrl_key = function() {
      var self = this;

      return self["native"].ctrlKey;
    };

    def.$meta_key = function() {
      var self = this;

      return self["native"].metaKey;
    };

    def.$alt_key = function() {
      var self = this;

      return self["native"].altKey;
    };

    def.$shift_key = function() {
      var self = this;

      return self["native"].shiftKey;
    };

    def.$key_code = function() {
      var self = this;

      return self["native"].keyCode;
    };

    def.$which = function() {
      var self = this;

      return self["native"].which;
    };

    Opal.defn(self, '$default_prevented?', def['$prevented?']);

    Opal.defn(self, '$prevent_default', def.$prevent);

    Opal.defn(self, '$propagation_stopped?', def['$stopped?']);

    Opal.defn(self, '$stop_propagation', def.$stop);

    return Opal.defn(self, '$stop_immediate_propagation', def.$stop_immediate);
  })(self, null);
};

/* Generated by Opal 0.7.2 */
Opal.modules["json"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $hash2 = Opal.hash2, $klass = Opal.klass;

  Opal.add_stubs(['$new', '$push', '$[]=', '$[]', '$create_id', '$json_create', '$attr_accessor', '$create_id=', '$===', '$parse', '$generate', '$from_object', '$to_json', '$responds_to?', '$to_io', '$write', '$to_s', '$to_a', '$strftime']);
  (function($base) {
    var self = $module($base, 'JSON');

    var def = self.$$proto, $scope = self.$$scope, $a, $b;

    
    var $parse  = JSON.parse,
        $hasOwn = Opal.hasOwnProperty;

    function to_opal(value, options) {
      switch (typeof value) {
        case 'string':
          return value;

        case 'number':
          return value;

        case 'boolean':
          return !!value;

        case 'null':
          return nil;

        case 'object':
          if (!value) return nil;

          if (value.$$is_array) {
            var arr = (options.array_class).$new();

            for (var i = 0, ii = value.length; i < ii; i++) {
              (arr).$push(to_opal(value[i], options));
            }

            return arr;
          }
          else {
            var hash = (options.object_class).$new();

            for (var k in value) {
              if ($hasOwn.call(value, k)) {
                (hash)['$[]='](k, to_opal(value[k], options));
              }
            }

            var klass;
            if ((klass = (hash)['$[]']($scope.get('JSON').$create_id())) != nil) {
              klass = Opal.cget(klass);
              return (klass).$json_create(hash);
            }
            else {
              return hash;
            }
          }
      }
    };
  

    (function(self) {
      var $scope = self.$$scope, def = self.$$proto;

      return self.$attr_accessor("create_id")
    })(self.$singleton_class());

    (($a = ["json_class"]), $b = self, $b['$create_id='].apply($b, $a), $a[$a.length-1]);

    Opal.defs(self, '$[]', function(value, options) {
      var $a, self = this;

      if (options == null) {
        options = $hash2([], {})
      }
      if ((($a = $scope.get('String')['$==='](value)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self.$parse(value, options)
        } else {
        return self.$generate(value, options)
      };
    });

    Opal.defs(self, '$parse', function(source, options) {
      var self = this;

      if (options == null) {
        options = $hash2([], {})
      }
      return self.$from_object($parse(source), options);
    });

    Opal.defs(self, '$parse!', function(source, options) {
      var self = this;

      if (options == null) {
        options = $hash2([], {})
      }
      return self.$parse(source, options);
    });

    Opal.defs(self, '$from_object', function(js_object, options) {
      var $a, $b, $c, self = this;

      if (options == null) {
        options = $hash2([], {})
      }
      ($a = "object_class", $b = options, ((($c = $b['$[]']($a)) !== false && $c !== nil) ? $c : $b['$[]=']($a, $scope.get('Hash'))));
      ($a = "array_class", $b = options, ((($c = $b['$[]']($a)) !== false && $c !== nil) ? $c : $b['$[]=']($a, $scope.get('Array'))));
      return to_opal(js_object, options.smap);
    });

    Opal.defs(self, '$generate', function(obj, options) {
      var self = this;

      if (options == null) {
        options = $hash2([], {})
      }
      return obj.$to_json(options);
    });

    Opal.defs(self, '$dump', function(obj, io, limit) {
      var $a, self = this, string = nil;

      if (io == null) {
        io = nil
      }
      if (limit == null) {
        limit = nil
      }
      string = self.$generate(obj);
      if (io !== false && io !== nil) {
        if ((($a = io['$responds_to?']("to_io")) !== nil && (!$a.$$is_boolean || $a == true))) {
          io = io.$to_io()};
        io.$write(string);
        return io;
        } else {
        return string
      };
    });
  })(self);
  (function($base, $super) {
    function $Object(){};
    var self = $Object = $klass($base, $super, 'Object', $Object);

    var def = self.$$proto, $scope = self.$$scope;

    return (Opal.defn(self, '$to_json', function() {
      var self = this;

      return self.$to_s().$to_json();
    }), nil) && 'to_json'
  })(self, null);
  (function($base) {
    var self = $module($base, 'Enumerable');

    var def = self.$$proto, $scope = self.$$scope;

    Opal.defn(self, '$to_json', function() {
      var self = this;

      return self.$to_a().$to_json();
    })
  })(self);
  (function($base, $super) {
    function $Array(){};
    var self = $Array = $klass($base, $super, 'Array', $Array);

    var def = self.$$proto, $scope = self.$$scope;

    return (def.$to_json = function() {
      var self = this;

      
      var result = [];

      for (var i = 0, length = self.length; i < length; i++) {
        result.push((self[i]).$to_json());
      }

      return '[' + result.join(', ') + ']';
    
    }, nil) && 'to_json'
  })(self, null);
  (function($base, $super) {
    function $Boolean(){};
    var self = $Boolean = $klass($base, $super, 'Boolean', $Boolean);

    var def = self.$$proto, $scope = self.$$scope;

    return (def.$to_json = function() {
      var self = this;

      return (self == true) ? 'true' : 'false';
    }, nil) && 'to_json'
  })(self, null);
  (function($base, $super) {
    function $Hash(){};
    var self = $Hash = $klass($base, $super, 'Hash', $Hash);

    var def = self.$$proto, $scope = self.$$scope;

    return (def.$to_json = function() {
      var self = this;

      
      var inspect = [],
          keys = self.keys,
          _map = self.map,
          smap = self.smap,
          map, khash;

      for (var i = 0, length = keys.length; i < length; i++) {
        var key = keys[i];

        if (key.$$is_string) {
          map = smap;
          khash = key;
        } else {
          map = _map;
          khash = key.$hash();
        }

        inspect.push((key).$to_s().$to_json() + ':' + (map[khash]).$to_json());
      }

      return '{' + inspect.join(', ') + '}';
    ;
    }, nil) && 'to_json'
  })(self, null);
  (function($base, $super) {
    function $NilClass(){};
    var self = $NilClass = $klass($base, $super, 'NilClass', $NilClass);

    var def = self.$$proto, $scope = self.$$scope;

    return (def.$to_json = function() {
      var self = this;

      return "null";
    }, nil) && 'to_json'
  })(self, null);
  (function($base, $super) {
    function $Numeric(){};
    var self = $Numeric = $klass($base, $super, 'Numeric', $Numeric);

    var def = self.$$proto, $scope = self.$$scope;

    return (def.$to_json = function() {
      var self = this;

      return self.toString();
    }, nil) && 'to_json'
  })(self, null);
  (function($base, $super) {
    function $String(){};
    var self = $String = $klass($base, $super, 'String', $String);

    var def = self.$$proto, $scope = self.$$scope;

    return Opal.defn(self, '$to_json', def.$inspect)
  })(self, null);
  (function($base, $super) {
    function $Time(){};
    var self = $Time = $klass($base, $super, 'Time', $Time);

    var def = self.$$proto, $scope = self.$$scope;

    return (def.$to_json = function() {
      var self = this;

      return self.$strftime("%FT%T%z").$to_json();
    }, nil) && 'to_json'
  })(self, null);
  return (function($base, $super) {
    function $Date(){};
    var self = $Date = $klass($base, $super, 'Date', $Date);

    var def = self.$$proto, $scope = self.$$scope;

    def.$to_json = function() {
      var self = this;

      return self.$to_s().$to_json();
    };

    return (def.$as_json = function() {
      var self = this;

      return self.$to_s();
    }, nil) && 'as_json';
  })(self, null);
};

/* Generated by Opal 0.7.2 */
Opal.modules["promise"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  Opal.add_stubs(['$resolve', '$new', '$reject', '$attr_reader', '$!', '$==', '$<<', '$>>', '$exception?', '$resolved?', '$value', '$rejected?', '$===', '$error', '$realized?', '$raise', '$^', '$call', '$resolve!', '$exception!', '$reject!', '$class', '$object_id', '$+', '$inspect', '$act?', '$prev', '$concat', '$it', '$lambda', '$reverse', '$<=', '$length', '$shift', '$-', '$each', '$wait', '$then', '$to_proc', '$map', '$reduce', '$always', '$try', '$tap', '$all?', '$find']);
  return (function($base, $super) {
    function $Promise(){};
    var self = $Promise = $klass($base, $super, 'Promise', $Promise);

    var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_2, TMP_3, TMP_4;

    def.success = def.exception = def.realized = def.delayed = def.failure = def.error = def.prev = def.next = def.value = nil;
    Opal.defs(self, '$value', function(value) {
      var self = this;

      return self.$new().$resolve(value);
    });

    Opal.defs(self, '$error', function(value) {
      var self = this;

      return self.$new().$reject(value);
    });

    Opal.defs(self, '$when', function(promises) {
      var self = this;

      promises = $slice.call(arguments, 0);
      return $scope.get('When').$new(promises);
    });

    self.$attr_reader("value", "error", "prev", "next");

    def.$initialize = function(success, failure) {
      var self = this;

      if (success == null) {
        success = nil
      }
      if (failure == null) {
        failure = nil
      }
      self.success = success;
      self.failure = failure;
      self.realized = nil;
      self.exception = false;
      self.value = nil;
      self.error = nil;
      self.delayed = nil;
      self.prev = nil;
      return self.next = nil;
    };

    def['$act?'] = function() {
      var self = this;

      return self.success['$=='](nil)['$!']();
    };

    def['$exception?'] = function() {
      var self = this;

      return self.exception;
    };

    def['$realized?'] = function() {
      var self = this;

      return self.realized['$=='](nil)['$!']();
    };

    def['$resolved?'] = function() {
      var self = this;

      return self.realized['$==']("resolve");
    };

    def['$rejected?'] = function() {
      var self = this;

      return self.realized['$==']("reject");
    };

    def['$^'] = function(promise) {
      var self = this;

      promise['$<<'](self);
      self['$>>'](promise);
      return promise;
    };

    def['$<<'] = function(promise) {
      var self = this;

      self.prev = promise;
      return self;
    };

    def['$>>'] = function(promise) {
      var $a, $b, $c, $d, self = this;

      self.next = promise;
      if ((($a = self['$exception?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
        promise.$reject(self.delayed)
      } else if ((($a = self['$resolved?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
        promise.$resolve(((($a = self.delayed) !== false && $a !== nil) ? $a : self.$value()))
      } else if ((($a = ($b = self['$rejected?'](), $b !== false && $b !== nil ?(((($c = self.failure['$!']()) !== false && $c !== nil) ? $c : $scope.get('Promise')['$===']((((($d = self.delayed) !== false && $d !== nil) ? $d : self.error))))) : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
        promise.$reject(((($a = self.delayed) !== false && $a !== nil) ? $a : self.$error()))};
      return self;
    };

    def.$resolve = function(value) {
      var $a, self = this, e = nil;

      if (value == null) {
        value = nil
      }
      if ((($a = self['$realized?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('ArgumentError'), "the promise has already been realized")};
      if ((($a = $scope.get('Promise')['$==='](value)) !== nil && (!$a.$$is_boolean || $a == true))) {
        value['$<<'](self.prev);
        return value['$^'](self);};
      self.realized = "resolve";
      self.value = value;
      try {
      if ((($a = self.success) !== nil && (!$a.$$is_boolean || $a == true))) {
          value = self.success.$call(value)};
        self['$resolve!'](value);
      } catch ($err) {if (Opal.rescue($err, [$scope.get('Exception')])) {e = $err;
        self['$exception!'](e)
        }else { throw $err; }
      };
      return self;
    };

    def['$resolve!'] = function(value) {
      var $a, self = this;

      if ((($a = self.next) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self.next.$resolve(value)
        } else {
        return self.delayed = value
      };
    };

    def.$reject = function(value) {
      var $a, self = this, e = nil;

      if (value == null) {
        value = nil
      }
      if ((($a = self['$realized?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('ArgumentError'), "the promise has already been realized")};
      if ((($a = $scope.get('Promise')['$==='](value)) !== nil && (!$a.$$is_boolean || $a == true))) {
        value['$<<'](self.prev);
        return value['$^'](self);};
      self.realized = "reject";
      self.error = value;
      try {
      if ((($a = self.failure) !== nil && (!$a.$$is_boolean || $a == true))) {
          value = self.failure.$call(value);
          if ((($a = $scope.get('Promise')['$==='](value)) !== nil && (!$a.$$is_boolean || $a == true))) {
            self['$reject!'](value)};
          } else {
          self['$reject!'](value)
        }
      } catch ($err) {if (Opal.rescue($err, [$scope.get('Exception')])) {e = $err;
        self['$exception!'](e)
        }else { throw $err; }
      };
      return self;
    };

    def['$reject!'] = function(value) {
      var $a, self = this;

      if ((($a = self.next) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self.next.$reject(value)
        } else {
        return self.delayed = value
      };
    };

    def['$exception!'] = function(error) {
      var self = this;

      self.exception = true;
      return self['$reject!'](error);
    };

    def.$then = TMP_1 = function() {
      var $a, self = this, $iter = TMP_1.$$p, block = $iter || nil;

      TMP_1.$$p = null;
      if ((($a = self.next) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('ArgumentError'), "a promise has already been chained")};
      return self['$^']($scope.get('Promise').$new(block));
    };

    Opal.defn(self, '$do', def.$then);

    def.$fail = TMP_2 = function() {
      var $a, self = this, $iter = TMP_2.$$p, block = $iter || nil;

      TMP_2.$$p = null;
      if ((($a = self.next) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('ArgumentError'), "a promise has already been chained")};
      return self['$^']($scope.get('Promise').$new(nil, block));
    };

    Opal.defn(self, '$rescue', def.$fail);

    Opal.defn(self, '$catch', def.$fail);

    def.$always = TMP_3 = function() {
      var $a, self = this, $iter = TMP_3.$$p, block = $iter || nil;

      TMP_3.$$p = null;
      if ((($a = self.next) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('ArgumentError'), "a promise has already been chained")};
      return self['$^']($scope.get('Promise').$new(block, block));
    };

    Opal.defn(self, '$finally', def.$always);

    Opal.defn(self, '$ensure', def.$always);

    def.$trace = TMP_4 = function(depth) {
      var $a, self = this, $iter = TMP_4.$$p, block = $iter || nil;

      if (depth == null) {
        depth = nil
      }
      TMP_4.$$p = null;
      if ((($a = self.next) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.$raise($scope.get('ArgumentError'), "a promise has already been chained")};
      return self['$^']($scope.get('Trace').$new(depth, block));
    };

    def.$inspect = function() {
      var $a, self = this, result = nil;

      result = "#<" + (self.$class()) + "(" + (self.$object_id()) + ")";
      if ((($a = self.next) !== nil && (!$a.$$is_boolean || $a == true))) {
        result = result['$+'](" >> " + (self.next.$inspect()))};
      if ((($a = self['$realized?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
        result = result['$+'](": " + ((((($a = self.value) !== false && $a !== nil) ? $a : self.error)).$inspect()) + ">")
        } else {
        result = result['$+'](">")
      };
      return result;
    };

    (function($base, $super) {
      function $Trace(){};
      var self = $Trace = $klass($base, $super, 'Trace', $Trace);

      var def = self.$$proto, $scope = self.$$scope, TMP_6;

      Opal.defs(self, '$it', function(promise) {
        var $a, self = this, current = nil, prev = nil;

        if ((($a = promise['$realized?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
          } else {
          self.$raise($scope.get('ArgumentError'), "the promise hasn't been realized")
        };
        current = (function() {if ((($a = promise['$act?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
          return [promise.$value()]
          } else {
          return []
        }; return nil; })();
        if ((($a = prev = promise.$prev()) !== nil && (!$a.$$is_boolean || $a == true))) {
          return current.$concat(self.$it(prev))
          } else {
          return current
        };
      });

      return (def.$initialize = TMP_6 = function(depth, block) {
        var $a, $b, TMP_5, self = this, $iter = TMP_6.$$p, $yield = $iter || nil;

        TMP_6.$$p = null;
        self.depth = depth;
        return Opal.find_super_dispatcher(self, 'initialize', TMP_6, null).apply(self, [($a = ($b = self).$lambda, $a.$$p = (TMP_5 = function(){var self = TMP_5.$$s || this, $a, $b, trace = nil;

        trace = $scope.get('Trace').$it(self).$reverse();
          if ((($a = (($b = depth !== false && depth !== nil) ? depth['$<='](trace.$length()) : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
            trace.$shift(trace.$length()['$-'](depth))};
          return ($a = block).$call.apply($a, [].concat(trace));}, TMP_5.$$s = self, TMP_5), $a).call($b)]);
      }, nil) && 'initialize';
    })(self, self);

    return (function($base, $super) {
      function $When(){};
      var self = $When = $klass($base, $super, 'When', $When);

      var def = self.$$proto, $scope = self.$$scope, TMP_7, TMP_9, TMP_11, TMP_13, TMP_17;

      def.wait = nil;
      def.$initialize = TMP_7 = function(promises) {
        var $a, $b, TMP_8, self = this, $iter = TMP_7.$$p, $yield = $iter || nil;

        if (promises == null) {
          promises = []
        }
        TMP_7.$$p = null;
        Opal.find_super_dispatcher(self, 'initialize', TMP_7, null).apply(self, []);
        self.wait = [];
        return ($a = ($b = promises).$each, $a.$$p = (TMP_8 = function(promise){var self = TMP_8.$$s || this;
if (promise == null) promise = nil;
        return self.$wait(promise)}, TMP_8.$$s = self, TMP_8), $a).call($b);
      };

      def.$each = TMP_9 = function() {
        var $a, $b, TMP_10, self = this, $iter = TMP_9.$$p, block = $iter || nil;

        TMP_9.$$p = null;
        if (block !== false && block !== nil) {
          } else {
          self.$raise($scope.get('ArgumentError'), "no block given")
        };
        return ($a = ($b = self).$then, $a.$$p = (TMP_10 = function(values){var self = TMP_10.$$s || this, $a, $b;
if (values == null) values = nil;
        return ($a = ($b = values).$each, $a.$$p = block.$to_proc(), $a).call($b)}, TMP_10.$$s = self, TMP_10), $a).call($b);
      };

      def.$collect = TMP_11 = function() {
        var $a, $b, TMP_12, self = this, $iter = TMP_11.$$p, block = $iter || nil;

        TMP_11.$$p = null;
        if (block !== false && block !== nil) {
          } else {
          self.$raise($scope.get('ArgumentError'), "no block given")
        };
        return ($a = ($b = self).$then, $a.$$p = (TMP_12 = function(values){var self = TMP_12.$$s || this, $a, $b;
if (values == null) values = nil;
        return $scope.get('When').$new(($a = ($b = values).$map, $a.$$p = block.$to_proc(), $a).call($b))}, TMP_12.$$s = self, TMP_12), $a).call($b);
      };

      def.$inject = TMP_13 = function(args) {
        var $a, $b, TMP_14, self = this, $iter = TMP_13.$$p, block = $iter || nil;

        args = $slice.call(arguments, 0);
        TMP_13.$$p = null;
        return ($a = ($b = self).$then, $a.$$p = (TMP_14 = function(values){var self = TMP_14.$$s || this, $a, $b;
if (values == null) values = nil;
        return ($a = ($b = values).$reduce, $a.$$p = block.$to_proc(), $a).apply($b, [].concat(args))}, TMP_14.$$s = self, TMP_14), $a).call($b);
      };

      Opal.defn(self, '$map', def.$collect);

      Opal.defn(self, '$reduce', def.$inject);

      def.$wait = function(promise) {
        var $a, $b, TMP_15, self = this;

        if ((($a = $scope.get('Promise')['$==='](promise)) !== nil && (!$a.$$is_boolean || $a == true))) {
          } else {
          promise = $scope.get('Promise').$value(promise)
        };
        if ((($a = promise['$act?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
          promise = promise.$then()};
        self.wait['$<<'](promise);
        ($a = ($b = promise).$always, $a.$$p = (TMP_15 = function(){var self = TMP_15.$$s || this, $a;
          if (self.next == null) self.next = nil;

        if ((($a = self.next) !== nil && (!$a.$$is_boolean || $a == true))) {
            return self.$try()
            } else {
            return nil
          }}, TMP_15.$$s = self, TMP_15), $a).call($b);
        return self;
      };

      Opal.defn(self, '$and', def.$wait);

      def['$>>'] = TMP_17 = function() {var $zuper = $slice.call(arguments, 0);
        var $a, $b, TMP_16, self = this, $iter = TMP_17.$$p, $yield = $iter || nil;

        TMP_17.$$p = null;
        return ($a = ($b = Opal.find_super_dispatcher(self, '>>', TMP_17, $iter).apply(self, $zuper)).$tap, $a.$$p = (TMP_16 = function(){var self = TMP_16.$$s || this;

        return self.$try()}, TMP_16.$$s = self, TMP_16), $a).call($b);
      };

      return (def.$try = function() {
        var $a, $b, $c, $d, self = this, promise = nil;

        if ((($a = ($b = ($c = self.wait)['$all?'], $b.$$p = "realized?".$to_proc(), $b).call($c)) !== nil && (!$a.$$is_boolean || $a == true))) {
          if ((($a = promise = ($b = ($d = self.wait).$find, $b.$$p = "rejected?".$to_proc(), $b).call($d)) !== nil && (!$a.$$is_boolean || $a == true))) {
            return self.$reject(promise.$error())
            } else {
            return self.$resolve(($a = ($b = self.wait).$map, $a.$$p = "value".$to_proc(), $a).call($b))
          }
          } else {
          return nil
        };
      }, nil) && 'try';
    })(self, self);
  })(self, null)
};

/* Generated by Opal 0.7.2 */
Opal.modules["opal/jquery/http"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass, $hash2 = Opal.hash2;

  Opal.add_stubs(['$require', '$to_n', '$each', '$define_singleton_method', '$send', '$new', '$define_method', '$attr_reader', '$delete', '$update', '$upcase', '$succeed', '$fail', '$promise', '$parse', '$private', '$tap', '$proc', '$ok?', '$resolve', '$reject', '$from_object', '$call']);
  self.$require("json");
  self.$require("native");
  self.$require("promise");
  self.$require("opal/jquery/constants");
  return (function($base, $super) {
    function $HTTP(){};
    var self = $HTTP = $klass($base, $super, 'HTTP', $HTTP);

    var def = self.$$proto, $scope = self.$$scope, $a, $b, TMP_1;

    def.settings = def.payload = def.url = def.method = def.handler = def.json = def.body = def.ok = def.xhr = def.promise = def.status_code = nil;
    var $ = $scope.get('JQUERY_SELECTOR').$to_n();

    Opal.cdecl($scope, 'ACTIONS', ["get", "post", "put", "delete", "patch", "head"]);

    ($a = ($b = $scope.get('ACTIONS')).$each, $a.$$p = (TMP_1 = function(action){var self = TMP_1.$$s || this, $a, $b, TMP_2, $c, TMP_3;
if (action == null) action = nil;
    ($a = ($b = self).$define_singleton_method, $a.$$p = (TMP_2 = function(url, options){var self = TMP_2.$$s || this, block;
if (url == null) url = nil;if (options == null) options = $hash2([], {});
        block = TMP_2.$$p || nil, TMP_2.$$p = null;
      return self.$new().$send(action, url, options, block)}, TMP_2.$$s = self, TMP_2), $a).call($b, action);
      return ($a = ($c = self).$define_method, $a.$$p = (TMP_3 = function(url, options){var self = TMP_3.$$s || this, block;
if (url == null) url = nil;if (options == null) options = $hash2([], {});
        block = TMP_3.$$p || nil, TMP_3.$$p = null;
      return self.$send(action, url, options, block)}, TMP_3.$$s = self, TMP_3), $a).call($c, action);}, TMP_1.$$s = self, TMP_1), $a).call($b);

    Opal.defs(self, '$setup', function() {
      var self = this;

      return $scope.get('Hash').$new($.ajaxSetup());
    });

    Opal.defs(self, '$setup=', function(settings) {
      var self = this;

      return $.ajaxSetup(settings.$to_n());
    });

    self.$attr_reader("body", "error_message", "method", "status_code", "url", "xhr");

    def.$initialize = function() {
      var self = this;

      self.settings = $hash2([], {});
      return self.ok = true;
    };

    def.$send = function(method, url, options, block) {
      var $a, self = this, settings = nil, payload = nil;

      self.method = method;
      self.url = url;
      self.payload = options.$delete("payload");
      self.handler = block;
      self.settings.$update(options);
      $a = [self.settings.$to_n(), self.payload], settings = $a[0], payload = $a[1];
      
      if (typeof(payload) === 'string') {
        settings.data = payload;
      }
      else if (payload != nil) {
        settings.data = payload.$to_json();
        settings.contentType = 'application/json';
      }

      settings.url  = self.url;
      settings.type = self.method.$upcase();

      settings.success = function(data, status, xhr) {
        return self.$succeed(data, status, xhr);
      };

      settings.error = function(xhr, status, error) {
        return self.$fail(xhr, status, error);
      };

      $.ajax(settings);
    ;
      if ((($a = self.handler) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self
        } else {
        return self.$promise()
      };
    };

    def.$json = function() {
      var $a, self = this;

      return ((($a = self.json) !== false && $a !== nil) ? $a : self.json = $scope.get('JSON').$parse(self.body));
    };

    def['$ok?'] = function() {
      var self = this;

      return self.ok;
    };

    def.$get_header = function(key) {
      var self = this;

      return self.xhr.getResponseHeader(key);;
    };

    self.$private();

    def.$promise = function() {
      var $a, $b, TMP_4, self = this;

      if ((($a = self.promise) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self.promise};
      return self.promise = ($a = ($b = $scope.get('Promise').$new()).$tap, $a.$$p = (TMP_4 = function(promise){var self = TMP_4.$$s || this, $a, $b, TMP_5;
if (promise == null) promise = nil;
      return self.handler = ($a = ($b = self).$proc, $a.$$p = (TMP_5 = function(res){var self = TMP_5.$$s || this, $a;
if (res == null) res = nil;
        if ((($a = res['$ok?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
            return promise.$resolve(res)
            } else {
            return promise.$reject(res)
          }}, TMP_5.$$s = self, TMP_5), $a).call($b)}, TMP_4.$$s = self, TMP_4), $a).call($b);
    };

    def.$succeed = function(data, status, xhr) {
      var $a, self = this;

      
      self.body = data;
      self.xhr  = xhr;
      self.status_code = xhr.status;

      if (typeof(data) === 'object') {
        self.json = $scope.get('JSON').$from_object(data);
      }
    ;
      if ((($a = self.handler) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self.handler.$call(self)
        } else {
        return nil
      };
    };

    return (def.$fail = function(xhr, status, error) {
      var $a, self = this;

      
      self.body = xhr.responseText;
      self.xhr = xhr;
      self.status_code = xhr.status;
    ;
      self.ok = false;
      if ((($a = self.handler) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self.handler.$call(self)
        } else {
        return nil
      };
    }, nil) && 'fail';
  })(self, null);
};

/* Generated by Opal 0.7.2 */
Opal.modules["opal/jquery/kernel"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module;

  return (function($base) {
    var self = $module($base, 'Kernel');

    var def = self.$$proto, $scope = self.$$scope;

    Opal.defn(self, '$alert', function(msg) {
      var self = this;

      alert(msg);
      return nil;
    })
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["opal/jquery"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice;

  Opal.add_stubs(['$==', '$require']);
  if ($scope.get('RUBY_ENGINE')['$==']("opal")) {
    self.$require("opal/jquery/window");
    self.$require("opal/jquery/document");
    self.$require("opal/jquery/element");
    self.$require("opal/jquery/event");
    self.$require("opal/jquery/http");
    return self.$require("opal/jquery/kernel");}
};

/* Generated by Opal 0.7.2 */
Opal.modules["opal-jquery"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice;

  Opal.add_stubs(['$require']);
  return self.$require("opal/jquery")
};

/* Generated by Opal 0.7.2 */
Opal.modules["paggio/utils"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass, $module = Opal.module;

  Opal.add_stubs(['$size', '$min', '$scan', '$gsub', '$proc', '$===', '$merge', '$to_proc', '$merge!']);
  return (function($base, $super) {
    function $Paggio(){};
    var self = $Paggio = $klass($base, $super, 'Paggio', $Paggio);

    var def = self.$$proto, $scope = self.$$scope;

    return (function($base) {
      var self = $module($base, 'Utils');

      var def = self.$$proto, $scope = self.$$scope;

      Opal.defs(self, '$heredoc', function(string) {
        var self = this, indent = nil;

        indent = (function() {try {return string.$scan(/^[ \t]*(?=\S)/).$min().$size() } catch ($err) { return 0 }})();
        return string.$gsub((new RegExp("^[ \\t]{" + indent + "}")), "");
      });

      Opal.defs(self, '$deep_merge', function(a, b) {
        var $a, $b, TMP_1, $c, self = this, merger = nil;

        merger = ($a = ($b = self).$proc, $a.$$p = (TMP_1 = function(key, v1, v2){var self = TMP_1.$$s || this, $a, $b;
if (key == null) key = nil;if (v1 == null) v1 = nil;if (v2 == null) v2 = nil;
        if ((($a = ($b = $scope.get('Hash')['$==='](v1), $b !== false && $b !== nil ?$scope.get('Hash')['$==='](v2) : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
            return ($a = ($b = v1).$merge, $a.$$p = merger.$to_proc(), $a).call($b, v2)
            } else {
            return v2
          }}, TMP_1.$$s = self, TMP_1), $a).call($b);
        return ($a = ($c = a).$merge, $a.$$p = merger.$to_proc(), $a).call($c, b);
      });

      Opal.defs(self, '$deep_merge!', function(a, b) {
        var $a, $b, TMP_2, $c, self = this, merger = nil;

        merger = ($a = ($b = self).$proc, $a.$$p = (TMP_2 = function(key, v1, v2){var self = TMP_2.$$s || this, $a, $b;
if (key == null) key = nil;if (v1 == null) v1 = nil;if (v2 == null) v2 = nil;
        if ((($a = ($b = $scope.get('Hash')['$==='](v1), $b !== false && $b !== nil ?$scope.get('Hash')['$==='](v2) : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
            ($a = ($b = v1)['$merge!'], $a.$$p = merger.$to_proc(), $a).call($b, v2);
            return v1;
            } else {
            return v2
          }}, TMP_2.$$s = self, TMP_2), $a).call($b);
        return ($a = ($c = a)['$merge!'], $a.$$p = merger.$to_proc(), $a).call($c, b);
      });
    })(self)
  })(self, null)
};

/* Generated by Opal 0.7.2 */
Opal.modules["paggio/html/helpers"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  Opal.add_stubs(['$define_method', '$instance_exec', '$to_proc', '$do', '$defhelper', '$[]=']);
  return (function($base, $super) {
    function $Paggio(){};
    var self = $Paggio = $klass($base, $super, 'Paggio', $Paggio);

    var def = self.$$proto, $scope = self.$$scope;

    return (function($base, $super) {
      function $HTML(){};
      var self = $HTML = $klass($base, $super, 'HTML', $HTML);

      var def = self.$$proto, $scope = self.$$scope;

      return (function($base, $super) {
        function $Element(){};
        var self = $Element = $klass($base, $super, 'Element', $Element);

        var def = self.$$proto, $scope = self.$$scope, TMP_1;

        Opal.defs(self, '$defhelper', TMP_1 = function(name) {
          var $a, $b, TMP_2, self = this, $iter = TMP_1.$$p, block = $iter || nil;

          TMP_1.$$p = null;
          return ($a = ($b = self).$define_method, $a.$$p = (TMP_2 = function(args){var self = TMP_2.$$s || this, body, $a, $b, $c;
args = $slice.call(arguments, 0);
            body = TMP_2.$$p || nil, TMP_2.$$p = null;
          ($a = ($b = self).$instance_exec, $a.$$p = block.$to_proc(), $a).apply($b, [].concat(args));
            if (body !== false && body !== nil) {
              ($a = ($c = self).$do, $a.$$p = body.$to_proc(), $a).call($c)};
            return self;}, TMP_2.$$s = self, TMP_2), $a).call($b, name);
        });

        return (Opal.defs(self, '$defhelper!', function(name, attribute) {
          var $a, $b, TMP_3, self = this;

          if (attribute == null) {
            attribute = name
          }
          return ($a = ($b = self).$defhelper, $a.$$p = (TMP_3 = function(){var self = TMP_3.$$s || this;
            if (self.attributes == null) self.attributes = nil;

          return self.attributes['$[]='](attribute, true)}, TMP_3.$$s = self, TMP_3), $a).call($b, "" + (name) + "!");
        }), nil) && 'defhelper!';
      })(self, $scope.get('BasicObject'))
    })(self, $scope.get('BasicObject'))
  })(self, null)
};

/* Generated by Opal 0.7.2 */
Opal.modules["paggio/html/element/a"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass, $hash2 = Opal.hash2;

  Opal.add_stubs(['$each', '$defhelper', '$[]=', '$to_s', '$defhelper!', '$<<']);
  return (function($base, $super) {
    function $Paggio(){};
    var self = $Paggio = $klass($base, $super, 'Paggio', $Paggio);

    var def = self.$$proto, $scope = self.$$scope;

    return (function($base, $super) {
      function $HTML(){};
      var self = $HTML = $klass($base, $super, 'HTML', $HTML);

      var def = self.$$proto, $scope = self.$$scope;

      return (function($base, $super) {
        function $Element(){};
        var self = $Element = $klass($base, $super, 'Element', $Element);

        var def = self.$$proto, $scope = self.$$scope;

        return (function($base, $super) {
          function $A(){};
          var self = $A = $klass($base, $super, 'A', $A);

          var def = self.$$proto, $scope = self.$$scope, $a, $b, TMP_1, $c, TMP_3;

          ($a = ($b = $hash2(["href", "url", "rel", "relative", "target", "type", "lang", "language", "media"], {"href": "href", "url": "href", "rel": "rel", "relative": "rel", "target": "target", "type": "type", "lang": "hreflang", "language": "hreflang", "media": "media"})).$each, $a.$$p = (TMP_1 = function(name, attribute){var self = TMP_1.$$s || this, $a, $b, TMP_2;
if (name == null) name = nil;if (attribute == null) attribute = nil;
          return ($a = ($b = self).$defhelper, $a.$$p = (TMP_2 = function(value){var self = TMP_2.$$s || this;
              if (self.attributes == null) self.attributes = nil;
if (value == null) value = nil;
            return self.attributes['$[]='](attribute, value.$to_s())}, TMP_2.$$s = self, TMP_2), $a).call($b, name)}, TMP_1.$$s = self, TMP_1), $a).call($b);

          self['$defhelper!']("download");

          self['$defhelper!']("ping");

          return ($a = ($c = self).$defhelper, $a.$$p = (TMP_3 = function(string){var self = TMP_3.$$s || this;
if (string == null) string = nil;
          return self['$<<'](string)}, TMP_3.$$s = self, TMP_3), $a).call($c, "text");
        })(self, self)
      })(self, $scope.get('BasicObject'))
    })(self, $scope.get('BasicObject'))
  })(self, null)
};

/* Generated by Opal 0.7.2 */
Opal.modules["paggio/html/element/base"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass, $hash2 = Opal.hash2;

  Opal.add_stubs(['$each', '$defhelper', '$[]=', '$to_s']);
  return (function($base, $super) {
    function $Paggio(){};
    var self = $Paggio = $klass($base, $super, 'Paggio', $Paggio);

    var def = self.$$proto, $scope = self.$$scope;

    return (function($base, $super) {
      function $HTML(){};
      var self = $HTML = $klass($base, $super, 'HTML', $HTML);

      var def = self.$$proto, $scope = self.$$scope;

      return (function($base, $super) {
        function $Element(){};
        var self = $Element = $klass($base, $super, 'Element', $Element);

        var def = self.$$proto, $scope = self.$$scope;

        return (function($base, $super) {
          function $Base(){};
          var self = $Base = $klass($base, $super, 'Base', $Base);

          var def = self.$$proto, $scope = self.$$scope, $a, $b, TMP_1;

          return ($a = ($b = $hash2(["href", "url", "target"], {"href": "href", "url": "href", "target": "target"})).$each, $a.$$p = (TMP_1 = function(name, attribute){var self = TMP_1.$$s || this, $a, $b, TMP_2;
if (name == null) name = nil;if (attribute == null) attribute = nil;
          return ($a = ($b = self).$defhelper, $a.$$p = (TMP_2 = function(value){var self = TMP_2.$$s || this;
              if (self.attributes == null) self.attributes = nil;
if (value == null) value = nil;
            return self.attributes['$[]='](attribute, value.$to_s())}, TMP_2.$$s = self, TMP_2), $a).call($b, name)}, TMP_1.$$s = self, TMP_1), $a).call($b)
        })(self, self)
      })(self, $scope.get('BasicObject'))
    })(self, $scope.get('BasicObject'))
  })(self, null)
};

/* Generated by Opal 0.7.2 */
Opal.modules["paggio/html/element/blockquote"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  Opal.add_stubs(['$defhelper', '$[]=', '$to_s']);
  return (function($base, $super) {
    function $Paggio(){};
    var self = $Paggio = $klass($base, $super, 'Paggio', $Paggio);

    var def = self.$$proto, $scope = self.$$scope;

    return (function($base, $super) {
      function $HTML(){};
      var self = $HTML = $klass($base, $super, 'HTML', $HTML);

      var def = self.$$proto, $scope = self.$$scope;

      return (function($base, $super) {
        function $Element(){};
        var self = $Element = $klass($base, $super, 'Element', $Element);

        var def = self.$$proto, $scope = self.$$scope;

        return (function($base, $super) {
          function $Blockquote(){};
          var self = $Blockquote = $klass($base, $super, 'Blockquote', $Blockquote);

          var def = self.$$proto, $scope = self.$$scope, $a, $b, TMP_1;

          return ($a = ($b = self).$defhelper, $a.$$p = (TMP_1 = function(value){var self = TMP_1.$$s || this;
            if (self.attributes == null) self.attributes = nil;
if (value == null) value = nil;
          return self.attributes['$[]=']("cite", value.$to_s())}, TMP_1.$$s = self, TMP_1), $a).call($b, "cite")
        })(self, self)
      })(self, $scope.get('BasicObject'))
    })(self, $scope.get('BasicObject'))
  })(self, null)
};

/* Generated by Opal 0.7.2 */
Opal.modules["paggio/html/element/button"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass, $hash2 = Opal.hash2;

  Opal.add_stubs(['$each', '$defhelper', '$[]=', '$attribute', '$to_s', '$defhelper!']);
  return (function($base, $super) {
    function $Paggio(){};
    var self = $Paggio = $klass($base, $super, 'Paggio', $Paggio);

    var def = self.$$proto, $scope = self.$$scope;

    return (function($base, $super) {
      function $HTML(){};
      var self = $HTML = $klass($base, $super, 'HTML', $HTML);

      var def = self.$$proto, $scope = self.$$scope;

      return (function($base, $super) {
        function $Element(){};
        var self = $Element = $klass($base, $super, 'Element', $Element);

        var def = self.$$proto, $scope = self.$$scope;

        return (function($base, $super) {
          function $Button(){};
          var self = $Button = $klass($base, $super, 'Button', $Button);

          var def = self.$$proto, $scope = self.$$scope, $a, $b, TMP_1;

          ($a = ($b = $hash2(["form", "name", "type", "value", "action", "encoding", "method", "target"], {"form": "form", "name": "name", "type": "type", "value": "value", "action": "formaction", "encoding": "formenctype", "method": "formmethod", "target": "formtarget"})).$each, $a.$$p = (TMP_1 = function(name, attributes){var self = TMP_1.$$s || this, $a, $b, TMP_2;
if (name == null) name = nil;if (attributes == null) attributes = nil;
          return ($a = ($b = self).$defhelper, $a.$$p = (TMP_2 = function(value){var self = TMP_2.$$s || this;
              if (self.attributes == null) self.attributes = nil;
if (value == null) value = nil;
            return self.attributes['$[]='](self.$attribute(), value.$to_s())}, TMP_2.$$s = self, TMP_2), $a).call($b, name)}, TMP_1.$$s = self, TMP_1), $a).call($b);

          self['$defhelper!']("autofocus");

          return self['$defhelper!']("disabled");
        })(self, self)
      })(self, $scope.get('BasicObject'))
    })(self, $scope.get('BasicObject'))
  })(self, null)
};

/* Generated by Opal 0.7.2 */
Opal.modules["paggio/html/element/canvas"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass, $hash2 = Opal.hash2;

  Opal.add_stubs(['$each', '$defhelper', '$[]=', '$to_s']);
  return (function($base, $super) {
    function $Paggio(){};
    var self = $Paggio = $klass($base, $super, 'Paggio', $Paggio);

    var def = self.$$proto, $scope = self.$$scope;

    return (function($base, $super) {
      function $HTML(){};
      var self = $HTML = $klass($base, $super, 'HTML', $HTML);

      var def = self.$$proto, $scope = self.$$scope;

      return (function($base, $super) {
        function $Element(){};
        var self = $Element = $klass($base, $super, 'Element', $Element);

        var def = self.$$proto, $scope = self.$$scope;

        return (function($base, $super) {
          function $Canvas(){};
          var self = $Canvas = $klass($base, $super, 'Canvas', $Canvas);

          var def = self.$$proto, $scope = self.$$scope, $a, $b, TMP_1;

          return ($a = ($b = $hash2(["width", "height"], {"width": "width", "height": "height"})).$each, $a.$$p = (TMP_1 = function(name, attribute){var self = TMP_1.$$s || this, $a, $b, TMP_2;
if (name == null) name = nil;if (attribute == null) attribute = nil;
          return ($a = ($b = self).$defhelper, $a.$$p = (TMP_2 = function(value){var self = TMP_2.$$s || this;
              if (self.attributes == null) self.attributes = nil;
if (value == null) value = nil;
            return self.attributes['$[]='](attribute, value.$to_s())}, TMP_2.$$s = self, TMP_2), $a).call($b, name)}, TMP_1.$$s = self, TMP_1), $a).call($b)
        })(self, self)
      })(self, $scope.get('BasicObject'))
    })(self, $scope.get('BasicObject'))
  })(self, null)
};

/* Generated by Opal 0.7.2 */
Opal.modules["paggio/html/element/img"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass, $hash2 = Opal.hash2;

  Opal.add_stubs(['$each', '$defhelper', '$[]=', '$to_s']);
  return (function($base, $super) {
    function $Paggio(){};
    var self = $Paggio = $klass($base, $super, 'Paggio', $Paggio);

    var def = self.$$proto, $scope = self.$$scope;

    return (function($base, $super) {
      function $HTML(){};
      var self = $HTML = $klass($base, $super, 'HTML', $HTML);

      var def = self.$$proto, $scope = self.$$scope;

      return (function($base, $super) {
        function $Element(){};
        var self = $Element = $klass($base, $super, 'Element', $Element);

        var def = self.$$proto, $scope = self.$$scope;

        return (function($base, $super) {
          function $Img(){};
          var self = $Img = $klass($base, $super, 'Img', $Img);

          var def = self.$$proto, $scope = self.$$scope, $a, $b, TMP_1, $c, TMP_3;

          ($a = ($b = $hash2(["src", "url", "alt", "description", "height", "width", "map"], {"src": "src", "url": "src", "alt": "alt", "description": "alt", "height": "height", "width": "width", "map": "usemap"})).$each, $a.$$p = (TMP_1 = function(name, attribute){var self = TMP_1.$$s || this, $a, $b, TMP_2;
if (name == null) name = nil;if (attribute == null) attribute = nil;
          return ($a = ($b = self).$defhelper, $a.$$p = (TMP_2 = function(value){var self = TMP_2.$$s || this;
              if (self.attributes == null) self.attributes = nil;
if (value == null) value = nil;
            return self.attributes['$[]='](attribute, value.$to_s())}, TMP_2.$$s = self, TMP_2), $a).call($b, name)}, TMP_1.$$s = self, TMP_1), $a).call($b);

          return ($a = ($c = self).$defhelper, $a.$$p = (TMP_3 = function(){var self = TMP_3.$$s || this;
            if (self.attributes == null) self.attributes = nil;

          return self.attributes['$[]=']("ismap", true)}, TMP_3.$$s = self, TMP_3), $a).call($c, "map!");
        })(self, self)
      })(self, $scope.get('BasicObject'))
    })(self, $scope.get('BasicObject'))
  })(self, null)
};

/* Generated by Opal 0.7.2 */
Opal.modules["paggio/html/element/input"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass, $hash2 = Opal.hash2;

  Opal.add_stubs(['$each', '$defhelper', '$[]=']);
  return (function($base, $super) {
    function $Paggio(){};
    var self = $Paggio = $klass($base, $super, 'Paggio', $Paggio);

    var def = self.$$proto, $scope = self.$$scope;

    return (function($base, $super) {
      function $HTML(){};
      var self = $HTML = $klass($base, $super, 'HTML', $HTML);

      var def = self.$$proto, $scope = self.$$scope;

      return (function($base, $super) {
        function $Element(){};
        var self = $Element = $klass($base, $super, 'Element', $Element);

        var def = self.$$proto, $scope = self.$$scope;

        return (function($base, $super) {
          function $Input(){};
          var self = $Input = $klass($base, $super, 'Input', $Input);

          var def = self.$$proto, $scope = self.$$scope, $a, $b, TMP_1;

          return ($a = ($b = $hash2(["type", "name", "value", "size", "place_holder", "read_only", "required"], {"type": "type", "name": "name", "value": "value", "size": "size", "place_holder": "placeholder", "read_only": "readonly", "required": "required"})).$each, $a.$$p = (TMP_1 = function(name, attribute){var self = TMP_1.$$s || this, $a, $b, TMP_2;
if (name == null) name = nil;if (attribute == null) attribute = nil;
          return ($a = ($b = self).$defhelper, $a.$$p = (TMP_2 = function(value){var self = TMP_2.$$s || this;
              if (self.attributes == null) self.attributes = nil;
if (value == null) value = nil;
            return self.attributes['$[]='](attribute, value)}, TMP_2.$$s = self, TMP_2), $a).call($b, name)}, TMP_1.$$s = self, TMP_1), $a).call($b)
        })(self, self)
      })(self, $scope.get('BasicObject'))
    })(self, $scope.get('BasicObject'))
  })(self, null)
};

/* Generated by Opal 0.7.2 */
Opal.modules["paggio/html/element/object"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass, $hash2 = Opal.hash2;

  Opal.add_stubs(['$each', '$defhelper', '$[]=']);
  return (function($base, $super) {
    function $Paggio(){};
    var self = $Paggio = $klass($base, $super, 'Paggio', $Paggio);

    var def = self.$$proto, $scope = self.$$scope;

    return (function($base, $super) {
      function $HTML(){};
      var self = $HTML = $klass($base, $super, 'HTML', $HTML);

      var def = self.$$proto, $scope = self.$$scope;

      return (function($base, $super) {
        function $Element(){};
        var self = $Element = $klass($base, $super, 'Element', $Element);

        var def = self.$$proto, $scope = self.$$scope;

        return (function($base, $super) {
          function $Object(){};
          var self = $Object = $klass($base, $super, 'Object', $Object);

          var def = self.$$proto, $scope = self.$$scope, $a, $b, TMP_1;

          return ($a = ($b = $hash2(["type", "data", "name", "height", "width"], {"type": "type", "data": "data", "name": "name", "height": "height", "width": "width"})).$each, $a.$$p = (TMP_1 = function(name, attribute){var self = TMP_1.$$s || this, $a, $b, TMP_2;
if (name == null) name = nil;if (attribute == null) attribute = nil;
          return ($a = ($b = self).$defhelper, $a.$$p = (TMP_2 = function(value){var self = TMP_2.$$s || this;
              if (self.attributes == null) self.attributes = nil;
if (value == null) value = nil;
            return self.attributes['$[]='](attribute, value)}, TMP_2.$$s = self, TMP_2), $a).call($b, name)}, TMP_1.$$s = self, TMP_1), $a).call($b)
        })(self, self)
      })(self, $scope.get('BasicObject'))
    })(self, $scope.get('BasicObject'))
  })(self, null)
};

/* Generated by Opal 0.7.2 */
Opal.modules["paggio/html/element/td"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  Opal.add_stubs(['$defhelper', '$[]=', '$to_s', '$join']);
  return (function($base, $super) {
    function $Paggio(){};
    var self = $Paggio = $klass($base, $super, 'Paggio', $Paggio);

    var def = self.$$proto, $scope = self.$$scope;

    return (function($base, $super) {
      function $HTML(){};
      var self = $HTML = $klass($base, $super, 'HTML', $HTML);

      var def = self.$$proto, $scope = self.$$scope;

      return (function($base, $super) {
        function $Element(){};
        var self = $Element = $klass($base, $super, 'Element', $Element);

        var def = self.$$proto, $scope = self.$$scope;

        return (function($base, $super) {
          function $Td(){};
          var self = $Td = $klass($base, $super, 'Td', $Td);

          var def = self.$$proto, $scope = self.$$scope, $a, $b, TMP_1, $c, TMP_2, $d, TMP_3;

          ($a = ($b = self).$defhelper, $a.$$p = (TMP_1 = function(value){var self = TMP_1.$$s || this;
            if (self.attributes == null) self.attributes = nil;
if (value == null) value = nil;
          return self.attributes['$[]=']("colspan", value.$to_s())}, TMP_1.$$s = self, TMP_1), $a).call($b, "columns");

          ($a = ($c = self).$defhelper, $a.$$p = (TMP_2 = function(value){var self = TMP_2.$$s || this;
            if (self.attributes == null) self.attributes = nil;
if (value == null) value = nil;
          return self.attributes['$[]=']("rowspan", value.$to_s())}, TMP_2.$$s = self, TMP_2), $a).call($c, "rows");

          return ($a = ($d = self).$defhelper, $a.$$p = (TMP_3 = function(args){var self = TMP_3.$$s || this;
            if (self.attributes == null) self.attributes = nil;
args = $slice.call(arguments, 0);
          return self.attributes['$[]=']("headers", args.$join(" "))}, TMP_3.$$s = self, TMP_3), $a).call($d, "headers");
        })(self, self)
      })(self, $scope.get('BasicObject'))
    })(self, $scope.get('BasicObject'))
  })(self, null)
};

/* Generated by Opal 0.7.2 */
Opal.modules["paggio/html/element"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass, $hash2 = Opal.hash2, $range = Opal.range;

  Opal.add_stubs(['$require', '$==', '$capitalize', '$const_defined?', '$new', '$const_get', '$each', '$to_proc', '$<<', '$heredoc', '$to_s', '$end_with?', '$[]=', '$[]', '$push', '$extend!', '$pop', '$join', '$defhelper', '$map', '$empty?', '$upcase', '$inspect']);
  self.$require("paggio/html/element/a");
  self.$require("paggio/html/element/base");
  self.$require("paggio/html/element/blockquote");
  self.$require("paggio/html/element/button");
  self.$require("paggio/html/element/canvas");
  self.$require("paggio/html/element/img");
  self.$require("paggio/html/element/input");
  self.$require("paggio/html/element/object");
  self.$require("paggio/html/element/td");
  return (function($base, $super) {
    function $Paggio(){};
    var self = $Paggio = $klass($base, $super, 'Paggio', $Paggio);

    var def = self.$$proto, $scope = self.$$scope;

    return (function($base, $super) {
      function $HTML(){};
      var self = $HTML = $klass($base, $super, 'HTML', $HTML);

      var def = self.$$proto, $scope = self.$$scope;

      return (function($base, $super) {
        function $Element(){};
        var self = $Element = $klass($base, $super, 'Element', $Element);

        var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_2, TMP_3, TMP_4, $a, $b, TMP_5, $c, TMP_7;

        def.children = def.attributes = def.class_names = def.owner = def.last = def.name = nil;
        Opal.defs(self, '$new', TMP_1 = function(owner, name, attributes) {var $zuper = $slice.call(arguments, 0);
          var $a, self = this, $iter = TMP_1.$$p, $yield = $iter || nil, const$ = nil;

          if (attributes == null) {
            attributes = $hash2([], {})
          }
          TMP_1.$$p = null;
          if (self['$==']($scope.get('Element'))) {
            } else {
            return Opal.find_super_dispatcher(self, 'new', TMP_1, $iter, $Element).apply(self, $zuper)
          };
          const$ = name.$capitalize();
          if ((($a = self['$const_defined?'](const$)) !== nil && (!$a.$$is_boolean || $a == true))) {
            return self.$const_get(const$).$new(owner, name, attributes)
            } else {
            return Opal.find_super_dispatcher(self, 'new', TMP_1, $iter, $Element).apply(self, $zuper)
          };
        });

        def.$initialize = function(owner, name, attributes) {
          var self = this;

          if (attributes == null) {
            attributes = $hash2([], {})
          }
          self.owner = owner;
          self.name = name;
          self.attributes = attributes;
          self.children = [];
          return self.class_names = [];
        };

        def.$each = TMP_2 = function() {
          var $a, $b, self = this, $iter = TMP_2.$$p, block = $iter || nil;

          TMP_2.$$p = null;
          return ($a = ($b = self.children).$each, $a.$$p = block.$to_proc(), $a).call($b);
        };

        def['$<<'] = function(what) {
          var self = this;

          self.children['$<<'](what);
          return self;
        };

        def.$method_missing = TMP_3 = function(name, content) {
          var $a, $b, self = this, $iter = TMP_3.$$p, block = $iter || nil;

          if (content == null) {
            content = nil
          }
          TMP_3.$$p = null;
          if (content !== false && content !== nil) {
            self['$<<'](((Opal.get('Paggio')).$$scope.get('Utils')).$heredoc(content.$to_s()))};
          if ((($a = name.$to_s()['$end_with?']("!")) !== nil && (!$a.$$is_boolean || $a == true))) {
            self.attributes['$[]=']("id", name['$[]']($range(0, -2, false)))
            } else {
            self.last = name;
            self.class_names.$push(name);
          };
          if (block !== false && block !== nil) {
            ($a = ($b = self.owner)['$extend!'], $a.$$p = block.$to_proc(), $a).call($b, self)};
          return self;
        };

        def['$[]'] = function(names) {
          var $a, self = this;

          names = $slice.call(arguments, 0);
          if ((($a = self.last) !== nil && (!$a.$$is_boolean || $a == true))) {
            } else {
            return nil
          };
          self.class_names.$pop();
          self.class_names.$push([self.last].concat(names).$join("-"));
          return self;
        };

        def.$do = TMP_4 = function() {
          var $a, $b, self = this, $iter = TMP_4.$$p, block = $iter || nil;

          TMP_4.$$p = null;
          ($a = ($b = self.owner)['$extend!'], $a.$$p = block.$to_proc(), $a).call($b, self);
          return self;
        };

        ($a = ($b = self).$defhelper, $a.$$p = (TMP_5 = function(hash){var self = TMP_5.$$s || this, $a, $b, TMP_6;
          if (self.attributes == null) self.attributes = nil;
if (hash == null) hash = nil;
        return self.attributes['$[]=']("style", ($a = ($b = hash).$map, $a.$$p = (TMP_6 = function(name, value){var self = TMP_6.$$s || this;
if (name == null) name = nil;if (value == null) value = nil;
          return "" + (name) + ": " + (value)}, TMP_6.$$s = self, TMP_6), $a).call($b).$join(";"))}, TMP_5.$$s = self, TMP_5), $a).call($b, "style");

        ($a = ($c = self).$defhelper, $a.$$p = (TMP_7 = function(hash){var self = TMP_7.$$s || this, $a, $b, TMP_8;
if (hash == null) hash = nil;
        return ($a = ($b = hash).$each, $a.$$p = (TMP_8 = function(name, value){var self = TMP_8.$$s || this;
            if (self.attributes == null) self.attributes = nil;
if (name == null) name = nil;if (value == null) value = nil;
          return self.attributes['$[]=']("data-" + (name), value.$to_s())}, TMP_8.$$s = self, TMP_8), $a).call($b)}, TMP_7.$$s = self, TMP_7), $a).call($c, "data");

        return (def.$inspect = function() {
          var $a, self = this;

          if ((($a = self.children['$empty?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
            return "#<HTML::Element(" + (self.name.$upcase()) + ")>"
            } else {
            return "#<HTML::Element(" + (self.name.$upcase()) + "): " + (self.children.$inspect()['$[]']($range(1, -2, false))) + ">"
          };
        }, nil) && 'inspect';
      })(self, $scope.get('BasicObject'))
    })(self, $scope.get('BasicObject'))
  })(self, null);
};

/* Generated by Opal 0.7.2 */
Opal.modules["paggio/html"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass, $range = Opal.range;

  Opal.add_stubs(['$require', '$attr_reader', '$raise', '$==', '$arity', '$instance_exec', '$to_proc', '$call', '$<<', '$first', '$===', '$instance_eval', '$each', '$end_with?', '$to_s', '$empty?', '$heredoc', '$shift', '$new', '$[]', '$inspect']);
  self.$require("paggio/html/helpers");
  self.$require("paggio/html/element");
  return (function($base, $super) {
    function $Paggio(){};
    var self = $Paggio = $klass($base, $super, 'Paggio', $Paggio);

    var def = self.$$proto, $scope = self.$$scope;

    return (function($base, $super) {
      function $HTML(){};
      var self = $HTML = $klass($base, $super, 'HTML', $HTML);

      var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_2, TMP_4, TMP_5;

      def.current = def.roots = def.version = nil;
      self.$attr_reader("version");

      def.$initialize = TMP_1 = function(version) {
        var $a, $b, self = this, $iter = TMP_1.$$p, block = $iter || nil;

        if (version == null) {
          version = 5
        }
        TMP_1.$$p = null;
        if (block !== false && block !== nil) {
          } else {
          Opal.get('Kernel').$raise(Opal.get('ArgumentError'), "no block given")
        };
        self.version = version;
        self.roots = [];
        self.current = nil;
        if (block.$arity()['$=='](0)) {
          return ($a = ($b = self).$instance_exec, $a.$$p = block.$to_proc(), $a).call($b)
          } else {
          return block.$call(self)
        };
      };

      def['$<<'] = function(what) {
        var $a, self = this;

        return (((($a = self.current) !== false && $a !== nil) ? $a : self.roots))['$<<'](what);
      };

      def['$root!'] = function() {
        var self = this;

        return self.roots.$first();
      };

      def['$roots!'] = function() {
        var self = this;

        return self.roots;
      };

      def['$element!'] = function() {
        var self = this;

        return self.current;
      };

      def['$extend!'] = TMP_2 = function(element) {
        var $a, $b, TMP_3, self = this, $iter = TMP_2.$$p, block = $iter || nil, old = nil, result = nil;

        if (element == null) {
          element = nil
        }
        TMP_2.$$p = null;
        $a = [self.current, element], old = $a[0], self.current = $a[1];
        result = block.$call(self);
        if ((($a = Opal.get('String')['$==='](result)) !== nil && (!$a.$$is_boolean || $a == true))) {
          ($a = ($b = self.current).$instance_eval, $a.$$p = (TMP_3 = function(){var self = TMP_3.$$s || this;

          return self.inner_html = result}, TMP_3.$$s = self, TMP_3), $a).call($b)};
        self.current = old;
        return self;
      };

      def.$each = TMP_4 = function() {
        var $a, $b, self = this, $iter = TMP_4.$$p, block = $iter || nil;

        TMP_4.$$p = null;
        return ($a = ($b = self.roots).$each, $a.$$p = block.$to_proc(), $a).call($b);
      };

      def.$method_missing = TMP_5 = function(name, args) {var $zuper = $slice.call(arguments, 0);
        var $a, $b, $c, TMP_6, self = this, $iter = TMP_5.$$p, block = $iter || nil, content = nil, element = nil, parent = nil, result = nil;

        args = $slice.call(arguments, 1);
        TMP_5.$$p = null;
        if ((($a = name.$to_s()['$end_with?']("!")) !== nil && (!$a.$$is_boolean || $a == true))) {
          return Opal.find_super_dispatcher(self, 'method_missing', TMP_5, $iter).apply(self, $zuper)};
        if ((($a = ((($b = args['$empty?']()) !== false && $b !== nil) ? $b : Opal.get('Hash')['$==='](args.$first()))) !== nil && (!$a.$$is_boolean || $a == true))) {
          } else {
          content = ((Opal.get('Paggio')).$$scope.get('Utils')).$heredoc(args.$shift().$to_s())
        };
        element = ($a = $scope.get('Element')).$new.apply($a, [self, name].concat(args));
        if (content !== false && content !== nil) {
          element['$<<'](content)};
        if (block !== false && block !== nil) {
          parent = self.current;
          self.current = element;
          result = block.$call(self);
          self.current = parent;
          if ((($b = Opal.get('String')['$==='](result)) !== nil && (!$b.$$is_boolean || $b == true))) {
            ($b = ($c = element).$instance_eval, $b.$$p = (TMP_6 = function(){var self = TMP_6.$$s || this;

            return self.inner_html = result}, TMP_6.$$s = self, TMP_6), $b).call($c)};};
        self['$<<'](element);
        return element;
      };

      return (def.$inspect = function() {
        var $a, self = this;

        if ((($a = self.roots['$empty?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
          return "#<HTML(" + (self.version) + ")>"
          } else {
          return "#<HTML(" + (self.version) + "): " + (self.roots.$inspect()['$[]']($range(1, -2, false))) + ">"
        };
      }, nil) && 'inspect';
    })(self, $scope.get('BasicObject'))
  })(self, null);
};

/* Generated by Opal 0.7.2 */
Opal.modules["paggio/css/unit"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var $a, $b, TMP_5, self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  Opal.add_stubs(['$map', '$to_proc', '$attr_reader', '$===', '$respond_to?', '$raise', '$class', '$to_u', '$new', '$==', '$convert', '$type', '$number', '$hash', '$each', '$define_method', '$+', '$compatible?', '$-', '$*', '$/', '$to_i', '$to_f', '$private', '$include?', '$class_eval', '$old_percent', '$match', '$[]', '$__send__', '$downcase']);
  (function($base, $super) {
    function $Paggio(){};
    var self = $Paggio = $klass($base, $super, 'Paggio', $Paggio);

    var def = self.$$proto, $scope = self.$$scope;

    return (function($base, $super) {
      function $CSS(){};
      var self = $CSS = $klass($base, $super, 'CSS', $CSS);

      var def = self.$$proto, $scope = self.$$scope;

      return (function($base, $super) {
        function $Unit(){};
        var self = $Unit = $klass($base, $super, 'Unit', $Unit);

        var def = self.$$proto, $scope = self.$$scope, $a, $b, $c, $d, TMP_1;

        def.type = def.number = nil;
        Opal.cdecl($scope, 'TYPES', ($a = ($b = ["em", "ex", "ch", "rem", "vh", "vw", "vmin", "vmax", "px", "mm", "cm", "in", "pt", "pc", "s", "deg"]).$map, $a.$$p = "to_sym".$to_proc(), $a).call($b));

        Opal.cdecl($scope, 'COMPATIBLE', ($a = ($c = ["in", "pt", "mm", "cm", "px", "pc"]).$map, $a.$$p = "to_sym".$to_proc(), $a).call($c));

        self.$attr_reader("type", "number");

        def.$initialize = function(number, type) {
          var self = this;

          self.number = number;
          return self.type = type;
        };

        def.$coerce = function(other) {
          var self = this;

          return [self, other];
        };

        def['$=='] = function(other) {
          var $a, self = this;

          if ((($a = $scope.get('Unit')['$==='](other)) !== nil && (!$a.$$is_boolean || $a == true))) {
            } else {
            if ((($a = other['$respond_to?']("to_u")) !== nil && (!$a.$$is_boolean || $a == true))) {
              } else {
              self.$raise($scope.get('TypeError'), "no implicit conversion of " + (other.$class()) + " into Unit")
            };
            other = other.$to_u();
          };
          if ((($a = $scope.get('Unit')['$==='](other)) !== nil && (!$a.$$is_boolean || $a == true))) {
            } else {
            other = $scope.get('Unit').$new(other, self.type)
          };
          return self.number['$=='](self.$convert(other, self.type));
        };

        def['$==='] = function(other) {
          var $a, self = this;

          return (($a = self.type['$=='](other.$type())) ? self.number['$=='](other.$number()) : $a);
        };

        Opal.defn(self, '$eql?', def['$==']);

        def.$hash = function() {
          var self = this;

          return [self.number, self.type].$hash();
        };

        ($a = ($d = $scope.get('TYPES')).$each, $a.$$p = (TMP_1 = function(name){var self = TMP_1.$$s || this, $a, $b, TMP_2;
if (name == null) name = nil;
        return ($a = ($b = self).$define_method, $a.$$p = (TMP_2 = function(){var self = TMP_2.$$s || this;

          return $scope.get('Unit').$new(self.$convert(self, name), name)}, TMP_2.$$s = self, TMP_2), $a).call($b, name)}, TMP_1.$$s = self, TMP_1), $a).call($d);

        def['$+'] = function(other) {
          var $a, $b, self = this;

          if ((($a = $scope.get('Unit')['$==='](other)) !== nil && (!$a.$$is_boolean || $a == true))) {
            } else {
            return $scope.get('Unit').$new(self.number['$+'](other), self.type)
          };
          if (self.type['$=='](other.$type())) {
            return $scope.get('Unit').$new(self.number['$+'](other.$number()), self.type)
          } else if ((($a = ($b = self['$compatible?'](self), $b !== false && $b !== nil ?self['$compatible?'](other) : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
            return $scope.get('Unit').$new(self.number['$+'](self.$convert(other, self.type)), self.type)
            } else {
            return self.$raise($scope.get('ArgumentError'), "" + (other.$type()) + " isn't compatible with " + (self.type))
          };
        };

        def['$-'] = function(other) {
          var $a, $b, self = this;

          if ((($a = $scope.get('Unit')['$==='](other)) !== nil && (!$a.$$is_boolean || $a == true))) {
            } else {
            return $scope.get('Unit').$new(self.number['$-'](other), self.type)
          };
          if (self.type['$=='](other.$type())) {
            return $scope.get('Unit').$new(self.number['$-'](other.$number()), self.type)
          } else if ((($a = ($b = self['$compatible?'](self), $b !== false && $b !== nil ?self['$compatible?'](other) : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
            return $scope.get('Unit').$new(self.number['$-'](self.$convert(other, self.type)), self.type)
            } else {
            return self.$raise($scope.get('ArgumentError'), "" + (other.$type()) + " isn't compatible with " + (self.type))
          };
        };

        def['$*'] = function(other) {
          var $a, $b, self = this;

          if ((($a = $scope.get('Unit')['$==='](other)) !== nil && (!$a.$$is_boolean || $a == true))) {
            } else {
            return $scope.get('Unit').$new(self.number['$*'](other), self.type)
          };
          if (self.type['$=='](other.$type())) {
            return $scope.get('Unit').$new(self.number['$*'](other.$number()), self.type)
          } else if ((($a = ($b = self['$compatible?'](self), $b !== false && $b !== nil ?self['$compatible?'](other) : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
            return $scope.get('Unit').$new(self.number['$*'](self.$convert(other, self.type)), self.type)
            } else {
            return self.$raise($scope.get('ArgumentError'), "" + (other.$type()) + " isn't compatible with " + (self.type))
          };
        };

        def['$/'] = function(other) {
          var $a, $b, self = this;

          if ((($a = $scope.get('Unit')['$==='](other)) !== nil && (!$a.$$is_boolean || $a == true))) {
            } else {
            return $scope.get('Unit').$new(self.number['$/'](other), self.type)
          };
          if (self.type['$=='](other.$type())) {
            return $scope.get('Unit').$new(self.number['$/'](other.$number()), self.type)
          } else if ((($a = ($b = self['$compatible?'](self), $b !== false && $b !== nil ?self['$compatible?'](other) : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
            return $scope.get('Unit').$new(self.number['$/'](self.$convert(other, self.type)), self.type)
            } else {
            return self.$raise($scope.get('ArgumentError'), "" + (other.$type()) + " isn't compatible with " + (self.type))
          };
        };

        def['$-@'] = function() {
          var self = this;

          return $scope.get('Unit').$new(self.number['$*'](-1), self.type);
        };

        def['$+@'] = function() {
          var self = this;

          return $scope.get('Unit').$new(self.number, self.type);
        };

        def.$to_i = function() {
          var self = this;

          return self.number.$to_i();
        };

        def.$to_f = function() {
          var self = this;

          return self.number.$to_f();
        };

        def.$to_u = function() {
          var self = this;

          return self;
        };

        def.$to_s = function() {
          var self = this;

          return "" + (self.number) + (self.type);
        };

        Opal.defn(self, '$to_str', def.$to_s);

        Opal.defn(self, '$inspect', def.$to_s);

        self.$private();

        def['$compatible?'] = function(unit) {
          var self = this;

          return $scope.get('COMPATIBLE')['$include?'](unit.$type());
        };

        return (def.$convert = function(unit, type) {
          var self = this, value = nil, px = nil, $case = nil;

          value = unit.$number();
          if (unit.$type()['$=='](type)) {
            return value};
          px = (function() {$case = unit.$type();if ("in"['$===']($case)) {return value['$*'](96)}else if ("pt"['$===']($case)) {return value['$*'](4.0)['$/'](3.0)}else if ("pc"['$===']($case)) {return value['$/'](12)['$*'](4.0)['$/'](3.0)}else if ("mm"['$===']($case)) {return value['$*'](3.77953)}else if ("cm"['$===']($case)) {return value['$*'](10)['$*'](3.77953)}else if ("px"['$===']($case)) {return value}else { return nil }})();
          return (function() {$case = type;if ("in"['$===']($case)) {return px['$/'](96.0)}else if ("pt"['$===']($case)) {return px['$/'](4.0)['$/'](3.0)}else if ("pc"['$===']($case)) {return px['$*'](12)['$/'](4.0)['$/'](3.0)}else if ("mm"['$===']($case)) {return px['$/'](3.77953)}else if ("cm"['$===']($case)) {return px['$/'](10)['$/'](3.77953)}else if ("px"['$===']($case)) {return px}else { return nil }})();
        }, nil) && 'convert';
      })(self, null)
    })(self, $scope.get('BasicObject'))
  })(self, null);
  (function($base, $super) {
    function $Numeric(){};
    var self = $Numeric = $klass($base, $super, 'Numeric', $Numeric);

    var def = self.$$proto, $scope = self.$$scope, $a, $b, TMP_3;

    ($a = ($b = (((((($scope.get('Paggio')).$$scope.get('CSS'))).$$scope.get('Unit'))).$$scope.get('TYPES'))).$each, $a.$$p = (TMP_3 = function(name){var self = TMP_3.$$s || this, $a, $b, TMP_4;
if (name == null) name = nil;
    return ($a = ($b = self).$define_method, $a.$$p = (TMP_4 = function(){var self = TMP_4.$$s || this;

      return (((($scope.get('Paggio')).$$scope.get('CSS'))).$$scope.get('Unit')).$new(self, name)}, TMP_4.$$s = self, TMP_4), $a).call($b, name)}, TMP_3.$$s = self, TMP_3), $a).call($b);

    return (def.$to_u = function() {
      var self = this;

      return self;
    }, nil) && 'to_u';
  })(self, null);
  ($a = ($b = [$scope.get('Fixnum'), $scope.get('Float')]).$each, $a.$$p = (TMP_5 = function(klass){var self = TMP_5.$$s || this, $a, $b, TMP_6;
if (klass == null) klass = nil;
  return ($a = ($b = klass).$class_eval, $a.$$p = (TMP_6 = function(){var self = TMP_6.$$s || this;

    self.$$proto.$old_percent = self.$$proto['$%'];
      return (Opal.defn(self, '$%', function(other) {
        var self = this;

        if (other == null) {
          other = nil
        }
        if (other !== false && other !== nil) {
          return self.$old_percent(other)
          } else {
          return (((($scope.get('Paggio')).$$scope.get('CSS'))).$$scope.get('Unit')).$new(self, "%")
        };
      }), nil) && '%';}, TMP_6.$$s = self, TMP_6), $a).call($b)}, TMP_5.$$s = self, TMP_5), $a).call($b);
  (function($base, $super) {
    function $String(){};
    var self = $String = $klass($base, $super, 'String', $String);

    var def = self.$$proto, $scope = self.$$scope;

    return (def.$to_u = function() {
      var $a, self = this, matches = nil, value = nil, unit = nil;

      if ((($a = matches = self.$match(/^([\d+.]+)(.+)?$/)) !== nil && (!$a.$$is_boolean || $a == true))) {
        value = matches['$[]'](1).$to_f();
        if ((($a = unit = matches['$[]'](2)) !== nil && (!$a.$$is_boolean || $a == true))) {
          return value.$__send__(unit.$downcase())
          } else {
          return value
        };
        } else {
        return 0
      };
    }, nil) && 'to_u'
  })(self, null);
  return (function($base, $super) {
    function $NilClass(){};
    var self = $NilClass = $klass($base, $super, 'NilClass', $NilClass);

    var def = self.$$proto, $scope = self.$$scope;

    return (def.$to_u = function() {
      var self = this;

      return 0;
    }, nil) && 'to_u'
  })(self, null);
};

/* Generated by Opal 0.7.2 */
Opal.modules["paggio/css/color"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  Opal.add_stubs(['$each', '$define_method', '$new', '$map', '$to_proc']);
  (function($base, $super) {
    function $Paggio(){};
    var self = $Paggio = $klass($base, $super, 'Paggio', $Paggio);

    var def = self.$$proto, $scope = self.$$scope;

    return (function($base, $super) {
      function $CSS(){};
      var self = $CSS = $klass($base, $super, 'CSS', $CSS);

      var def = self.$$proto, $scope = self.$$scope;

      return (function($base, $super) {
        function $Color(){};
        var self = $Color = $klass($base, $super, 'Color', $Color);

        var def = self.$$proto, $scope = self.$$scope;

        return (def.$initialize = function(value, type) {
          var self = this;

          self.internal = value;
          return self.type = type;
        }, nil) && 'initialize'
      })(self, null)
    })(self, $scope.get('BasicObject'))
  })(self, null);
  (function($base, $super) {
    function $String(){};
    var self = $String = $klass($base, $super, 'String', $String);

    var def = self.$$proto, $scope = self.$$scope, $a, $b, TMP_1, $c, $d;

    return ($a = ($b = ($c = ($d = ["rgb", "rgba", "hsl", "hsla"]).$map, $c.$$p = "to_sym".$to_proc(), $c).call($d)).$each, $a.$$p = (TMP_1 = function(name){var self = TMP_1.$$s || this, $a, $b, TMP_2;
if (name == null) name = nil;
    return ($a = ($b = self).$define_method, $a.$$p = (TMP_2 = function(){var self = TMP_2.$$s || this;

      return (((($scope.get('Paggio')).$$scope.get('CSS'))).$$scope.get('Color')).$new(self, name)}, TMP_2.$$s = self, TMP_2), $a).call($b, name)}, TMP_1.$$s = self, TMP_1), $a).call($b)
  })(self, null);
  return (function($base, $super) {
    function $Array(){};
    var self = $Array = $klass($base, $super, 'Array', $Array);

    var def = self.$$proto, $scope = self.$$scope, $a, $b, TMP_3, $c, $d;

    return ($a = ($b = ($c = ($d = ["rgb", "rgba", "hsl", "hsla"]).$map, $c.$$p = "to_sym".$to_proc(), $c).call($d)).$each, $a.$$p = (TMP_3 = function(name){var self = TMP_3.$$s || this, $a, $b, TMP_4;
if (name == null) name = nil;
    return ($a = ($b = self).$define_method, $a.$$p = (TMP_4 = function(){var self = TMP_4.$$s || this;

      return (((($scope.get('Paggio')).$$scope.get('CSS'))).$$scope.get('Color')).$new(self, name)}, TMP_4.$$s = self, TMP_4), $a).call($b, name)}, TMP_3.$$s = self, TMP_3), $a).call($b)
  })(self, null);
};

/* Generated by Opal 0.7.2 */
Opal.modules["paggio/css/definition"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass, $range = Opal.range, $hash2 = Opal.hash2;

  Opal.add_stubs(['$new', '$==', '$arity', '$instance_exec', '$to_proc', '$call', '$empty?', '$each', '$inspect', '$===', '$first', '$>', '$length', '$raise', '$style', '$name', '$value', '$[]', '$join', '$map', '$to_i', '$*', '$to_s', '$end_with?', '$respond_to?', '$__send__', '$<<', '$last', '$pop', '$!', '$other', '$shift', '$horizontal?', '$private']);
  return (function($base, $super) {
    function $Paggio(){};
    var self = $Paggio = $klass($base, $super, 'Paggio', $Paggio);

    var def = self.$$proto, $scope = self.$$scope;

    return (function($base, $super) {
      function $CSS(){};
      var self = $CSS = $klass($base, $super, 'CSS', $CSS);

      var def = self.$$proto, $scope = self.$$scope;

      return (function($base, $super) {
        function $Definition(){};
        var self = $Definition = $klass($base, $super, 'Definition', $Definition);

        var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_2, TMP_11;

        def.style = def.important = nil;
        Opal.cdecl($scope, 'Style', Opal.get('Struct').$new("name", "value", "important"));

        def.$initialize = TMP_1 = function() {
          var $a, $b, self = this, $iter = TMP_1.$$p, block = $iter || nil;

          TMP_1.$$p = null;
          self.style = [];
          if (block !== false && block !== nil) {
            if (block.$arity()['$=='](0)) {
              return ($a = ($b = self).$instance_exec, $a.$$p = block.$to_proc(), $a).call($b)
              } else {
              return block.$call(self)
            }
            } else {
            return nil
          };
        };

        def['$empty?'] = function() {
          var self = this;

          return self.style['$empty?']();
        };

        def.$each = TMP_2 = function() {
          var $a, $b, self = this, $iter = TMP_2.$$p, block = $iter || nil;

          TMP_2.$$p = null;
          return ($a = ($b = self.style).$each, $a.$$p = block.$to_proc(), $a).call($b);
        };

        def.$gradient = function(args) {
          var $a, self = this;

          args = $slice.call(arguments, 0);
          return ($a = $scope.get('Gradient')).$new.apply($a, [].concat(args));
        };

        def.$url = function(arg) {
          var self = this;

          return "url(" + (arg.$inspect()) + ")";
        };

        def.$background = function(args) {
          var $a, $b, TMP_3, $c, TMP_4, self = this;

          args = $slice.call(arguments, 0);
          if ((($a = $scope.get('Gradient')['$==='](args.$first())) !== nil && (!$a.$$is_boolean || $a == true))) {
            if (args.$length()['$>'](1)) {
              self.$raise($scope.get('NotImplementedError'), "multiple gradients not implemented yet")};
            return ($a = ($b = args.$first()).$each, $a.$$p = (TMP_3 = function(s){var self = TMP_3.$$s || this, $a;
if (s == null) s = nil;
            return self.$style(((($a = s.$name()) !== false && $a !== nil) ? $a : "background-image"), s.$value())}, TMP_3.$$s = self, TMP_3), $a).call($b);
          } else if ((($a = Opal.get('Hash')['$==='](args.$first())) !== nil && (!$a.$$is_boolean || $a == true))) {
            return ($a = ($c = args.$first()).$each, $a.$$p = (TMP_4 = function(sub, value){var self = TMP_4.$$s || this;
if (sub == null) sub = nil;if (value == null) value = nil;
            return self.$style("background-" + (sub), value)}, TMP_4.$$s = self, TMP_4), $a).call($c)
            } else {
            return self.$style("background", args)
          };
        };

        def.$border = function(args) {
          var $a, $b, TMP_5, self = this, options = nil;

          args = $slice.call(arguments, 0);
          if ((($a = Opal.get('Hash')['$==='](args.$first())) !== nil && (!$a.$$is_boolean || $a == true))) {
            if (args.$length()['$=='](1)) {
              options = args.$first()};
            return ($a = ($b = options).$each, $a.$$p = (TMP_5 = function(name, value){var self = TMP_5.$$s || this, $a, $b, TMP_6, $c, TMP_8, $case = nil;
if (name == null) name = nil;if (value == null) value = nil;
            return (function() {$case = name;if ("radius"['$===']($case)) {if ((($a = Opal.get('Hash')['$==='](value)) !== nil && (!$a.$$is_boolean || $a == true))) {
                return ($a = ($b = value).$each, $a.$$p = (TMP_6 = function(horizontal, value){var self = TMP_6.$$s || this, $a, $b, TMP_7;
if (horizontal == null) horizontal = nil;if (value == null) value = nil;
                return ($a = ($b = value).$each, $a.$$p = (TMP_7 = function(vertical, value){var self = TMP_7.$$s || this;
if (vertical == null) vertical = nil;if (value == null) value = nil;
                  self.$style("-moz-border-radius-" + (horizontal) + (vertical), value);
                    self.$style("-webkit-border-" + (horizontal) + "-" + (vertical) + "-radius", value);
                    return self.$style("border-" + (horizontal) + "-" + (vertical) + "-radius", value);}, TMP_7.$$s = self, TMP_7), $a).call($b)}, TMP_6.$$s = self, TMP_6), $a).call($b)
                } else {
                self.$style("-moz-border-radius", value);
                self.$style("-webkit-border-radius", value);
                return self.$style("border-radius", value);
              }}else if ("color"['$===']($case)) {if ((($a = Opal.get('Hash')['$==='](value)) !== nil && (!$a.$$is_boolean || $a == true))) {
                return ($a = ($c = value).$each, $a.$$p = (TMP_8 = function(name, value){var self = TMP_8.$$s || this;
if (name == null) name = nil;if (value == null) value = nil;
                return self.$style("border-" + (name) + "-color", value)}, TMP_8.$$s = self, TMP_8), $a).call($c)
                } else {
                return self.$style("border-color", value)
              }}else {return self.$style("border-" + (name), value)}})()}, TMP_5.$$s = self, TMP_5), $a).call($b);
            } else {
            return self.$style("border", args)
          };
        };

        def.$box = function(options) {
          var $a, $b, TMP_9, self = this;

          if ((($a = Opal.get('Hash')['$==='](options)) !== nil && (!$a.$$is_boolean || $a == true))) {
            return ($a = ($b = options).$each, $a.$$p = (TMP_9 = function(name, value){var self = TMP_9.$$s || this, $a, $b, TMP_10, $case = nil;
if (name == null) name = nil;if (value == null) value = nil;
            return (function() {$case = name;if ("shadow"['$===']($case)) {if ((($a = Opal.get('Array')['$==='](value)) !== nil && (!$a.$$is_boolean || $a == true))) {
                if ((($a = Opal.get('Array')['$==='](value['$[]'](0))) !== nil && (!$a.$$is_boolean || $a == true))) {
                  value = ($a = ($b = value).$map, $a.$$p = (TMP_10 = function(v){var self = TMP_10.$$s || this;
if (v == null) v = nil;
                  return v.$join(" ")}, TMP_10.$$s = self, TMP_10), $a).call($b).$join(", ")
                  } else {
                  value = value.$join(" ")
                }};
              self.$style("-moz-box-shadow", value);
              self.$style("-webkit-box-shadow", value);
              return self.$style("box-shadow", value);}else {return self.$style("box-" + (name), value)}})()}, TMP_9.$$s = self, TMP_9), $a).call($b)
            } else {
            return self.$style("box", options)
          };
        };

        def.$opacity = function(value) {
          var self = this;

          self.$style("opacity", value);
          self.$style("-moz-opacity", value);
          self.$style("-ms-filter", "\"progid:DXImageTransform.Microsoft.Alpha(Opacity=" + ((value['$*'](100)).$to_i()) + ")\"");
          return self.$style("filter", "alpha(opacity=" + ((value['$*'](100)).$to_i()) + ")");
        };

        def.$animation = function(args) {
          var self = this;

          args = $slice.call(arguments, 0);
          self.$style("animation", args);
          return self.$style("-webkit-animation", args);
        };

        def.$transition = function(args) {
          var self = this;

          args = $slice.call(arguments, 0);
          self.$style("transition", args);
          self.$style("-webkit-transition", args);
          return self.$style("-moz-transition", args);
        };

        def.$method_missing = TMP_11 = function(name, args) {
          var $a, $b, $c, TMP_12, self = this, $iter = TMP_11.$$p, block = $iter || nil, important = nil, argument = nil;

          args = $slice.call(arguments, 1);
          TMP_11.$$p = null;
          name = name.$to_s();
          important = name['$end_with?']("!");
          if (important !== false && important !== nil) {
            name = name['$[]']($range(0, -2, false))};
          if (important !== false && important !== nil) {
            self.important = true};
          if ((($a = (($b = important !== false && important !== nil) ? self['$respond_to?'](name) : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
            ($a = ($b = self).$__send__, $a.$$p = block.$to_proc(), $a).apply($b, [name].concat(args));
            self.important = false;
            return nil;};
          if (args.$length()['$=='](1)) {
            argument = args.$first();
            if ((($a = Opal.get('Hash')['$==='](argument)) !== nil && (!$a.$$is_boolean || $a == true))) {
              ($a = ($c = argument).$each, $a.$$p = (TMP_12 = function(sub, value){var self = TMP_12.$$s || this;
if (sub == null) sub = nil;if (value == null) value = nil;
              return self.$style("" + (name) + "-" + (sub), value)}, TMP_12.$$s = self, TMP_12), $a).call($c)
              } else {
              self.$style(name, argument)
            };
            } else {
            self.$style(name, args.$join(" "))
          };
          self.important = false;
          return self;
        };

        def.$style = function(name, value, important) {
          var $a, self = this;

          if (value == null) {
            value = nil
          }
          if (important == null) {
            important = self.important
          }
          if ((($a = Opal.get('Array')['$==='](value)) !== nil && (!$a.$$is_boolean || $a == true))) {
            value = value.$join(" ")};
          if ((($a = $scope.get('Style')['$==='](name)) !== nil && (!$a.$$is_boolean || $a == true))) {
            return self.style['$<<'](name)
            } else {
            return self.style['$<<']($scope.get('Style').$new(name, value, important))
          };
        };

        def['$style!'] = function(name, value) {
          var self = this;

          if (value == null) {
            value = nil
          }
          return self.$style(name, value, true);
        };

        return (function($base, $super) {
          function $Gradient(){};
          var self = $Gradient = $klass($base, $super, 'Gradient', $Gradient);

          var def = self.$$proto, $scope = self.$$scope, TMP_13;

          def.to = def.from = def.start = def.end = nil;
          def.$initialize = function(args) {
            var $a, $b, self = this, options = nil;

            args = $slice.call(arguments, 0);
            options = (function() {if ((($a = Opal.get('Hash')['$==='](args.$last())) !== nil && (!$a.$$is_boolean || $a == true))) {
              return args.$pop()
              } else {
              return $hash2([], {})
            }; return nil; })();
            self.to = options['$[]']("to");
            self.from = options['$[]']("from");
            if ((($a = ($b = self.to, $b !== false && $b !== nil ?self.from['$!']() : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
              self.from = self.$other(self.to)
            } else if ((($a = ($b = self.from, $b !== false && $b !== nil ?self.to['$!']() : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
              self.to = self.$other(self.from)};
            self.start = args.$shift();
            return self.end = args.$shift();
          };

          def.$each = TMP_13 = function() {
            var $a, self = this, $iter = TMP_13.$$p, block = $iter || nil;

            TMP_13.$$p = null;
            block.$call(self.$style("-moz-linear-gradient(" + (self.to) + ", " + (self.start) + " 0%, " + (self.end) + " 100%)"));
            if ((($a = self['$horizontal?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
              block.$call(self.$style("-webkit-gradient(linear, " + (self.from) + " top, " + (self.to) + " top, color-stop(0%, " + (self.start) + "), color-stop(100%, " + (self.end) + "))"))
              } else {
              block.$call(self.$style("-webkit-gradient(linear, left " + (self.from) + ", left " + (self.to) + ", color-stop(0%, " + (self.start) + "), color-stop(100%, " + (self.end) + "))"))
            };
            block.$call(self.$style("-webkit-linear-gradient(" + (self.to) + ", " + (self.start) + " 0%, " + (self.end) + " 100%)"));
            block.$call(self.$style("-o-linear-gradient(" + (self.to) + ", " + (self.start) + " 0%, " + (self.end) + " 100%)"));
            block.$call(self.$style("-ms-linear-gradient(" + (self.to) + ", " + (self.start) + " 0%, " + (self.end) + " 100%)"));
            return block.$call(self.$style("linear-gradient(to " + (self.to) + ", " + (self.start) + " 0%, " + (self.end) + " 100%)"));
          };

          def['$horizontal?'] = function() {
            var $a, self = this;

            return ((($a = self.to['$==']("left")) !== false && $a !== nil) ? $a : self.to['$==']("right"));
          };

          def['$vertical?'] = function() {
            var $a, self = this;

            return ((($a = self.to['$==']("top")) !== false && $a !== nil) ? $a : self.to['$==']("bottom"));
          };

          self.$private();

          def.$other = function(side) {
            var self = this, $case = nil;

            return (function() {$case = side;if ("left"['$===']($case)) {return "right"}else if ("right"['$===']($case)) {return "left"}else if ("top"['$===']($case)) {return "bottom"}else if ("bottom"['$===']($case)) {return "top"}else { return nil }})();
          };

          return (def.$style = function(args) {
            var $a, self = this;

            args = $slice.call(arguments, 0);
            if (args.$length()['$=='](1)) {
              return $scope.get('Style').$new(nil, args.$first())
              } else {
              return ($a = $scope.get('Style')).$new.apply($a, [].concat(args))
            };
          }, nil) && 'style';
        })(self, null);
      })(self, $scope.get('BasicObject'))
    })(self, $scope.get('BasicObject'))
  })(self, null)
};

/* Generated by Opal 0.7.2 */
Opal.modules["paggio/css"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass, $range = Opal.range;

  Opal.add_stubs(['$require', '$new', '$each', '$start_with?', '$+', '$[]', '$==', '$attr_reader', '$raise', '$arity', '$instance_exec', '$to_proc', '$call', '$any?', '$include?', '$<<', '$selector', '$pop', '$__send__', '$definition', '$last']);
  self.$require("paggio/css/unit");
  self.$require("paggio/css/color");
  self.$require("paggio/css/definition");
  return (function($base, $super) {
    function $Paggio(){};
    var self = $Paggio = $klass($base, $super, 'Paggio', $Paggio);

    var def = self.$$proto, $scope = self.$$scope;

    (function($base, $super) {
      function $CSS(){};
      var self = $CSS = $klass($base, $super, 'CSS', $CSS);

      var def = self.$$proto, $scope = self.$$scope, TMP_2, TMP_3, TMP_6;

      def.current = nil;
      Opal.cdecl($scope, 'Rule', Opal.get('Struct').$new("selector", "definition"));

      Opal.defs(self, '$selector', function(list) {
        var $a, $b, TMP_1, self = this, result = nil;

        result = "";
        ($a = ($b = list).$each, $a.$$p = (TMP_1 = function(part){var self = TMP_1.$$s || this, $a;
if (part == null) part = nil;
        if ((($a = part['$start_with?']("&")) !== nil && (!$a.$$is_boolean || $a == true))) {
            return result = result['$+'](part['$[]']($range(1, -1, false)))
            } else {
            return result = result['$+'](" "['$+'](part))
          }}, TMP_1.$$s = self, TMP_1), $a).call($b);
        if (result['$[]'](0)['$=='](" ")) {
          return result['$[]']($range(1, -1, false))
          } else {
          return result
        };
      });

      self.$attr_reader("rules");

      def.$initialize = TMP_2 = function() {
        var $a, $b, self = this, $iter = TMP_2.$$p, block = $iter || nil;

        TMP_2.$$p = null;
        if (block !== false && block !== nil) {
          } else {
          Opal.get('Kernel').$raise(Opal.get('ArgumentError'), "no block given")
        };
        self.selector = [];
        self.current = [];
        self.rules = [];
        if (block.$arity()['$=='](0)) {
          return ($a = ($b = self).$instance_exec, $a.$$p = block.$to_proc(), $a).call($b)
          } else {
          return block.$call(self)
        };
      };

      def.$rule = TMP_3 = function(names) {
        var $a, $b, $c, TMP_4, TMP_5, self = this, $iter = TMP_3.$$p, block = $iter || nil;

        names = $slice.call(arguments, 0);
        TMP_3.$$p = null;
        if (block !== false && block !== nil) {
          } else {
          return nil
        };
        if ((($a = ($b = ($c = names)['$any?'], $b.$$p = (TMP_4 = function(n){var self = TMP_4.$$s || this;
if (n == null) n = nil;
        return n['$include?'](",")}, TMP_4.$$s = self, TMP_4), $b).call($c)) !== nil && (!$a.$$is_boolean || $a == true))) {
          Opal.get('Kernel').$raise(Opal.get('ArgumentError'), "selectors cannot contain commas")};
        return ($a = ($b = names).$each, $a.$$p = (TMP_5 = function(name){var self = TMP_5.$$s || this;
          if (self.selector == null) self.selector = nil;
          if (self.current == null) self.current = nil;
          if (self.rules == null) self.rules = nil;
if (name == null) name = nil;
        self.selector['$<<'](name);
          self.current['$<<']($scope.get('Rule').$new($scope.get('CSS').$selector(self.selector), $scope.get('Definition').$new()));
          block.$call(self);
          self.selector.$pop();
          return self.rules['$<<'](self.current.$pop());}, TMP_5.$$s = self, TMP_5), $a).call($b);
      };

      return (def.$method_missing = TMP_6 = function(name, args) {
        var $a, $b, self = this, $iter = TMP_6.$$p, block = $iter || nil;

        args = $slice.call(arguments, 1);
        TMP_6.$$p = null;
        return ($a = ($b = self.current.$last().$definition()).$__send__, $a.$$p = block.$to_proc(), $a).apply($b, [name].concat(args));
      }, nil) && 'method_missing';
    })(self, $scope.get('BasicObject'));

    return (function($base, $super) {
      function $HTML(){};
      var self = $HTML = $klass($base, $super, 'HTML', $HTML);

      var def = self.$$proto, $scope = self.$$scope, TMP_7;

      def.current = def.roots = nil;
      return (def.$style = TMP_7 = function() {
        var $a, $b, self = this, $iter = TMP_7.$$p, block = $iter || nil;

        TMP_7.$$p = null;
        return (((($a = self.current) !== false && $a !== nil) ? $a : self.roots))['$<<'](($a = ($b = $scope.get('CSS')).$new, $a.$$p = block.$to_proc(), $a).call($b));
      }, nil) && 'style'
    })(self, $scope.get('BasicObject'));
  })(self, null);
};

/* Generated by Opal 0.7.2 */
Opal.modules["stringio"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass, $range = Opal.range;

  Opal.add_stubs(['$include', '$new', '$call', '$close', '$attr_accessor', '$length', '$include?', '$!', '$check_readable', '$==', '$===', '$>=', '$raise', '$>', '$+', '$-', '$seek', '$enum_for', '$eof?', '$ord', '$[]', '$check_writable', '$String', '$write', '$closed_write?', '$closed_read?']);
  return (function($base, $super) {
    function $StringIO(){};
    var self = $StringIO = $klass($base, $super, 'StringIO', $StringIO);

    var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_2, TMP_3;

    def.position = def.string = def.closed = nil;
    self.$include((($scope.get('IO')).$$scope.get('Readable')));

    self.$include((($scope.get('IO')).$$scope.get('Writable')));

    Opal.defs(self, '$open', TMP_1 = function(string, mode) {
      var self = this, $iter = TMP_1.$$p, block = $iter || nil, io = nil, res = nil;

      if (string == null) {
        string = ""
      }
      if (mode == null) {
        mode = nil
      }
      TMP_1.$$p = null;
      io = self.$new(string, mode);
      res = block.$call(io);
      io.$close();
      return res;
    });

    self.$attr_accessor("string");

    def.$initialize = function(string, mode) {
      var $a, $b, self = this;

      if (string == null) {
        string = ""
      }
      if (mode == null) {
        mode = "rw"
      }
      self.string = string;
      self.position = string.$length();
      if ((($a = ($b = mode['$include?']("r"), $b !== false && $b !== nil ?mode['$include?']("w")['$!']() : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self.closed = "write"
      } else if ((($a = ($b = mode['$include?']("w"), $b !== false && $b !== nil ?mode['$include?']("r")['$!']() : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self.closed = "read"
        } else {
        return nil
      };
    };

    def['$eof?'] = function() {
      var self = this;

      self.$check_readable();
      return self.position['$=='](self.string.$length());
    };

    Opal.defn(self, '$eof', def['$eof?']);

    def.$seek = function(pos, whence) {
      var self = this, $case = nil;

      if (whence == null) {
        whence = (($scope.get('IO')).$$scope.get('SEEK_SET'))
      }
      $case = whence;if ((($scope.get('IO')).$$scope.get('SEEK_SET'))['$===']($case)) {if (pos['$>='](0)) {
        } else {
        self.$raise((($scope.get('Errno')).$$scope.get('EINVAL')))
      };
      self.position = pos;}else if ((($scope.get('IO')).$$scope.get('SEEK_CUR'))['$===']($case)) {if (self.position['$+'](pos)['$>'](self.string.$length())) {
        self.position = self.string.$length()
        } else {
        self.position = self.position['$+'](pos)
      }}else if ((($scope.get('IO')).$$scope.get('SEEK_END'))['$===']($case)) {if (pos['$>'](self.string.$length())) {
        self.position = 0
        } else {
        self.position = self.position['$-'](pos)
      }};
      return 0;
    };

    def.$tell = function() {
      var self = this;

      return self.position;
    };

    Opal.defn(self, '$pos', def.$tell);

    Opal.defn(self, '$pos=', def.$seek);

    def.$rewind = function() {
      var self = this;

      return self.$seek(0);
    };

    def.$each_byte = TMP_2 = function() {
      var $a, $b, self = this, $iter = TMP_2.$$p, block = $iter || nil, i = nil;

      TMP_2.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        return self.$enum_for("each_byte")
      };
      self.$check_readable();
      i = self.position;
      while (!((($b = self['$eof?']()) !== nil && (!$b.$$is_boolean || $b == true)))) {
      block.$call(self.string['$[]'](i).$ord());
      i = i['$+'](1);};
      return self;
    };

    def.$each_char = TMP_3 = function() {
      var $a, $b, self = this, $iter = TMP_3.$$p, block = $iter || nil, i = nil;

      TMP_3.$$p = null;
      if (block !== false && block !== nil) {
        } else {
        return self.$enum_for("each_char")
      };
      self.$check_readable();
      i = self.position;
      while (!((($b = self['$eof?']()) !== nil && (!$b.$$is_boolean || $b == true)))) {
      block.$call(self.string['$[]'](i));
      i = i['$+'](1);};
      return self;
    };

    def.$write = function(string) {
      var self = this, before = nil, after = nil;

      self.$check_writable();
      string = self.$String(string);
      if (self.string.$length()['$=='](self.position)) {
        self.string = self.string['$+'](string);
        return self.position = self.position['$+'](string.$length());
        } else {
        before = self.string['$[]']($range(0, self.position['$-'](1), false));
        after = self.string['$[]']($range(self.position['$+'](string.$length()), -1, false));
        self.string = before['$+'](string)['$+'](after);
        return self.position = self.position['$+'](string.$length());
      };
    };

    def.$read = function(length, outbuf) {
      var $a, self = this, string = nil, str = nil;

      if (length == null) {
        length = nil
      }
      if (outbuf == null) {
        outbuf = nil
      }
      self.$check_readable();
      if ((($a = self['$eof?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
        return nil};
      string = (function() {if (length !== false && length !== nil) {
        str = self.string['$[]'](self.position, length);
        self.position = self.position['$+'](length);
        return str;
        } else {
        str = self.string['$[]']($range(self.position, -1, false));
        self.position = self.string.$length();
        return str;
      }; return nil; })();
      if (outbuf !== false && outbuf !== nil) {
        return outbuf.$write(string)
        } else {
        return string
      };
    };

    def.$close = function() {
      var self = this;

      return self.closed = "both";
    };

    def.$close_read = function() {
      var self = this;

      if (self.closed['$==']("write")) {
        return self.closed = "both"
        } else {
        return self.closed = "read"
      };
    };

    def.$close_write = function() {
      var self = this;

      if (self.closed['$==']("read")) {
        return self.closed = "both"
        } else {
        return self.closed = "write"
      };
    };

    def['$closed?'] = function() {
      var self = this;

      return self.closed['$==']("both");
    };

    def['$closed_read?'] = function() {
      var $a, self = this;

      return ((($a = self.closed['$==']("read")) !== false && $a !== nil) ? $a : self.closed['$==']("both"));
    };

    def['$closed_write?'] = function() {
      var $a, self = this;

      return ((($a = self.closed['$==']("write")) !== false && $a !== nil) ? $a : self.closed['$==']("both"));
    };

    def.$check_writable = function() {
      var $a, self = this;

      if ((($a = self['$closed_write?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self.$raise($scope.get('IOError'), "not opened for writing")
        } else {
        return nil
      };
    };

    return (def.$check_readable = function() {
      var $a, self = this;

      if ((($a = self['$closed_read?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self.$raise($scope.get('IOError'), "not opened for reading")
        } else {
        return nil
      };
    }, nil) && 'check_readable';
  })(self, $scope.get('IO'))
};

/* Generated by Opal 0.7.2 */
Opal.modules["paggio/formatter"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass, $hash2 = Opal.hash2;

  Opal.add_stubs(['$require', '$[]=', '$to_h', '$[]', '$dup', '$deep_merge!', '$call', '$replace', '$===', '$new', '$merge', '$each', '$string', '$indent?', '$+', '$-', '$puts', '$*', '$chomp', '$lines', '$print', '$gsub', '$to_s', '$for', '$version', '$indent', '$format', '$instance_eval', '$empty?', '$map', '$escape', '$<<', '$join', '$definition', '$selector', '$name', '$value', '$important', '$reverse', '$rules']);
  self.$require("stringio");
  return (function($base, $super) {
    function $Paggio(){};
    var self = $Paggio = $klass($base, $super, 'Paggio', $Paggio);

    var def = self.$$proto, $scope = self.$$scope, $a, $b, TMP_7, $c, TMP_10, $d, TMP_17;

    (function($base, $super) {
      function $Formatter(){};
      var self = $Formatter = $klass($base, $super, 'Formatter', $Formatter);

      var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_2, TMP_4, TMP_5;

      def.options = def.io = nil;
      Opal.defs(self, '$to_h', function() {
        var $a, self = this;
        if (self.formatters == null) self.formatters = nil;

        return ((($a = self.formatters) !== false && $a !== nil) ? $a : self.formatters = $hash2([], {}));
      });

      Opal.defs(self, '$for', TMP_1 = function(klass) {
        var self = this, $iter = TMP_1.$$p, block = $iter || nil;

        TMP_1.$$p = null;
        if (block !== false && block !== nil) {
          return self.$to_h()['$[]='](klass, block)
          } else {
          return self.$to_h()['$[]'](klass)
        };
      });

      Opal.defs(self, '$options', TMP_2 = function(options) {
        var self = this, $iter = TMP_2.$$p, block = $iter || nil, old = nil, result = nil;

        TMP_2.$$p = null;
        old = $scope.get('OPTIONS').$dup();
        $scope.get('Utils')['$deep_merge!']($scope.get('OPTIONS'), options);
        result = block.$call();
        $scope.get('OPTIONS').$replace(old);
        return result;
      });

      Opal.cdecl($scope, 'OPTIONS', $hash2(["indent"], {"indent": $hash2(["level", "with"], {"level": 0, "with": "\t"})}));

      def.$initialize = function(io, options) {
        var $a, self = this;

        if (io == null) {
          io = nil
        }
        if (options == null) {
          options = $hash2([], {})
        }
        if ((($a = $scope.get('Hash')['$==='](io)) !== nil && (!$a.$$is_boolean || $a == true))) {
          self.io = $scope.get('StringIO').$new();
          self.options = io;
          } else {
          self.io = ((($a = io) !== false && $a !== nil) ? $a : $scope.get('StringIO').$new());
          self.options = options;
        };
        return self.options = $scope.get('OPTIONS').$merge(self.options);
      };

      def.$format = function(item) {
        var $a, $b, TMP_3, self = this;

        ($a = ($b = $scope.get('Formatter').$to_h()).$each, $a.$$p = (TMP_3 = function(klass, block){var self = TMP_3.$$s || this, $a;
if (klass == null) klass = nil;if (block == null) block = nil;
        if ((($a = klass['$==='](item)) !== nil && (!$a.$$is_boolean || $a == true))) {
            block.$call(self, item);
            return ($breaker.$v = nil, $breaker);
            } else {
            return nil
          }}, TMP_3.$$s = self, TMP_3), $a).call($b);
        return self;
      };

      def.$to_s = function() {
        var self = this;

        return self.io.$string();
      };

      def['$indent?'] = TMP_4 = function() {
        var self = this, $iter = TMP_4.$$p, block = $iter || nil;

        TMP_4.$$p = null;
        try {
        return self.options['$[]']("indent")['$[]']("level")
        } catch ($err) {if (true) {
          return false
          }else { throw $err; }
        };
      };

      def.$indent = TMP_5 = function() {
        var $a, $b, self = this, $iter = TMP_5.$$p, block = $iter || nil;

        TMP_5.$$p = null;
        if ((($a = self['$indent?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
          ($a = "level", $b = self.options['$[]']("indent"), $b['$[]=']($a, $b['$[]']($a)['$+'](1)));
          block.$call();
          return ($a = "level", $b = self.options['$[]']("indent"), $b['$[]=']($a, $b['$[]']($a)['$-'](1)));
          } else {
          return block.$call()
        };
      };

      def.$print = function(text) {
        var $a, $b, TMP_6, self = this, level = nil;

        if ((($a = level = self['$indent?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
          return ($a = ($b = text.$lines()).$each, $a.$$p = (TMP_6 = function(line){var self = TMP_6.$$s || this;
            if (self.io == null) self.io = nil;
            if (self.options == null) self.options = nil;
if (line == null) line = nil;
          return self.io.$puts("" + (self.options['$[]']("indent")['$[]']("with")['$*'](level)) + (line.$chomp()))}, TMP_6.$$s = self, TMP_6), $a).call($b)
          } else {
          return self.io.$print(text)
        };
      };

      return (def.$escape = function(string) {
        var self = this;

        return string.$to_s().$gsub(/["><']|&(?!([a-zA-Z]+|(#\d+));)/, $hash2(["&", ">", "<", "\"", "'"], {"&": "&amp;", ">": "&gt;", "<": "&lt;", "\"": "&quot;", "'": "&#39;"}));
      }, nil) && 'escape';
    })(self, null);

    ($a = ($b = $scope.get('Formatter')).$for, $a.$$p = (TMP_7 = function(f, item){var self = TMP_7.$$s || this, $a, $b, TMP_8, $case = nil;
if (f == null) f = nil;if (item == null) item = nil;
    $case = item.$version();if ((5)['$===']($case)) {f.$print("<!DOCTYPE html>")};
      f.$print("<html>");
      ($a = ($b = f).$indent, $a.$$p = (TMP_8 = function(){var self = TMP_8.$$s || this, $a, $b, TMP_9;

      return ($a = ($b = item).$each, $a.$$p = (TMP_9 = function(root){var self = TMP_9.$$s || this;
if (root == null) root = nil;
        return f.$format(root)}, TMP_9.$$s = self, TMP_9), $a).call($b)}, TMP_8.$$s = self, TMP_8), $a).call($b);
      return f.$print("</html>");}, TMP_7.$$s = self, TMP_7), $a).call($b, $scope.get('HTML'));

    ($a = ($c = $scope.get('Formatter')).$for, $a.$$p = (TMP_10 = function(f, item){var self = TMP_10.$$s || this, $a, $b, $c, TMP_11, TMP_12, $d, TMP_13, name = nil, attributes = nil, class_names = nil, attrs = nil;
if (f == null) f = nil;if (item == null) item = nil;
    $a = Opal.to_ary(($b = ($c = item).$instance_eval, $b.$$p = (TMP_11 = function(){var self = TMP_11.$$s || this;
        if (self.name == null) self.name = nil;
        if (self.attributes == null) self.attributes = nil;
        if (self.class_names == null) self.class_names = nil;

      return [self.name, self.attributes, self.class_names]}, TMP_11.$$s = self, TMP_11), $b).call($c)), name = ($a[0] == null ? nil : $a[0]), attributes = ($a[1] == null ? nil : $a[1]), class_names = ($a[2] == null ? nil : $a[2]);
      if ((($a = ($b = attributes['$empty?'](), $b !== false && $b !== nil ?class_names['$empty?']() : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
        f.$print("<" + (name) + ">")
        } else {
        attrs = ($a = ($b = attributes).$map, $a.$$p = (TMP_12 = function(key, value){var self = TMP_12.$$s || this;
if (key == null) key = nil;if (value == null) value = nil;
        return "" + (f.$escape(key)) + "=\"" + (f.$escape(value)) + "\""}, TMP_12.$$s = self, TMP_12), $a).call($b);
        if ((($a = class_names['$empty?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
          } else {
          attrs['$<<']("class=\"" + (f.$escape(class_names.$join(" "))) + "\"")
        };
        f.$print("<" + (name) + " " + (attrs.$join(" ")) + ">");
      };
      ($a = ($d = f).$indent, $a.$$p = (TMP_13 = function(){var self = TMP_13.$$s || this, $a, $b, $c, TMP_14, TMP_15, inner = nil;

      if ((($a = inner = ($b = ($c = item).$instance_eval, $b.$$p = (TMP_14 = function(){var self = TMP_14.$$s || this;
          if (self.inner_html == null) self.inner_html = nil;

        return self.inner_html}, TMP_14.$$s = self, TMP_14), $b).call($c)) !== nil && (!$a.$$is_boolean || $a == true))) {
          return f.$print(inner)
          } else {
          return ($a = ($b = item).$each, $a.$$p = (TMP_15 = function(child){var self = TMP_15.$$s || this, $a, $b, TMP_16, $case = nil;
if (child == null) child = nil;
          return (function() {$case = child;if ($scope.get('String')['$===']($case)) {return f.$print(f.$escape(child))}else if ($scope.get('CSS')['$===']($case)) {f.$print("<style>");
            ($a = ($b = f).$indent, $a.$$p = (TMP_16 = function(){var self = TMP_16.$$s || this;

            return f.$format(child)}, TMP_16.$$s = self, TMP_16), $a).call($b);
            return f.$print("</style>");}else {return f.$format(child)}})()}, TMP_15.$$s = self, TMP_15), $a).call($b)
        }}, TMP_13.$$s = self, TMP_13), $a).call($d);
      return f.$print("</" + (name) + ">");}, TMP_10.$$s = self, TMP_10), $a).call($c, (($scope.get('HTML')).$$scope.get('Element')));

    return ($a = ($d = $scope.get('Formatter')).$for, $a.$$p = (TMP_17 = function(f, item){var self = TMP_17.$$s || this, $a, $b, TMP_18;
if (f == null) f = nil;if (item == null) item = nil;
    return ($a = ($b = item.$rules().$reverse()).$each, $a.$$p = (TMP_18 = function(rule){var self = TMP_18.$$s || this, $a, $b, TMP_19;
if (rule == null) rule = nil;
      if ((($a = rule.$definition()['$empty?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
          return nil;};
        f.$print("" + (rule.$selector()) + " {");
        ($a = ($b = f).$indent, $a.$$p = (TMP_19 = function(){var self = TMP_19.$$s || this, $a, $b, TMP_20;

        return ($a = ($b = rule.$definition()).$each, $a.$$p = (TMP_20 = function(style){var self = TMP_20.$$s || this, $a;
if (style == null) style = nil;
          return f.$print("" + (style.$name()) + ": " + (style.$value()) + ((function() {if ((($a = style.$important()) !== nil && (!$a.$$is_boolean || $a == true))) {
              return " !important"
              } else {
              return nil
            }; return nil; })()) + ";")}, TMP_20.$$s = self, TMP_20), $a).call($b)}, TMP_19.$$s = self, TMP_19), $a).call($b);
        return f.$print("}");}, TMP_18.$$s = self, TMP_18), $a).call($b)}, TMP_17.$$s = self, TMP_17), $a).call($d, $scope.get('CSS'));
  })(self, null);
};

/* Generated by Opal 0.7.2 */
Opal.modules["paggio"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass, $hash2 = Opal.hash2;

  Opal.add_stubs(['$require', '$options', '$to_proc', '$to_s', '$format', '$new', '$tap', '$each']);
  self.$require("paggio/utils");
  self.$require("paggio/html");
  self.$require("paggio/css");
  self.$require("paggio/formatter");
  return (function($base, $super) {
    function $Paggio(){};
    var self = $Paggio = $klass($base, $super, 'Paggio', $Paggio);

    var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_2, TMP_3, TMP_4, TMP_5;

    Opal.defs(self, '$options', TMP_1 = function(options) {
      var $a, $b, self = this, $iter = TMP_1.$$p, block = $iter || nil;

      TMP_1.$$p = null;
      return ($a = ($b = $scope.get('Formatter')).$options, $a.$$p = block.$to_proc(), $a).call($b, options);
    });

    Opal.defs(self, '$indent', TMP_2 = function(options) {
      var $a, $b, self = this, $iter = TMP_2.$$p, block = $iter || nil;

      TMP_2.$$p = null;
      return ($a = ($b = self).$options, $a.$$p = block.$to_proc(), $a).call($b, $hash2(["indent"], {"indent": options}));
    });

    Opal.defs(self, '$css', TMP_3 = function(args) {
      var $a, $b, self = this, $iter = TMP_3.$$p, block = $iter || nil;

      args = $slice.call(arguments, 0);
      TMP_3.$$p = null;
      return $scope.get('Formatter').$new().$format(($a = ($b = $scope.get('CSS')).$new, $a.$$p = block.$to_proc(), $a).apply($b, [].concat(args))).$to_s();
    });

    Opal.defs(self, '$html', TMP_4 = function(args) {
      var $a, $b, self = this, $iter = TMP_4.$$p, block = $iter || nil;

      args = $slice.call(arguments, 0);
      TMP_4.$$p = null;
      return $scope.get('Formatter').$new().$format(($a = ($b = $scope.get('HTML')).$new, $a.$$p = block.$to_proc(), $a).apply($b, [].concat(args))).$to_s();
    });

    return (Opal.defs(self, '$html!', TMP_5 = function() {
      var $a, $b, TMP_6, self = this, $iter = TMP_5.$$p, block = $iter || nil;

      TMP_5.$$p = null;
      return ($a = ($b = $scope.get('Formatter').$new()).$tap, $a.$$p = (TMP_6 = function(f){var self = TMP_6.$$s || this, $a, $b, TMP_7, $c, $d;
if (f == null) f = nil;
      return ($a = ($b = ($c = ($d = $scope.get('HTML')).$new, $c.$$p = block.$to_proc(), $c).call($d)).$each, $a.$$p = (TMP_7 = function(root){var self = TMP_7.$$s || this;
if (root == null) root = nil;
        return f.$format(root)}, TMP_7.$$s = self, TMP_7), $a).call($b)}, TMP_6.$$s = self, TMP_6), $a).call($b).$to_s();
    }), nil) && 'html!';
  })(self, null);
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/version"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module;

  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    Opal.cdecl($scope, 'VERSION', "0.1.0.beta1")
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/utils"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass;

  Opal.add_stubs(['$new', '$[]', '$map', '$split', '$decode_uri_component', '$join', '$encode_uri_component', '$to_s', '$encode_uri']);
  (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    Opal.cdecl($scope, 'Size', $scope.get('Struct').$new("width", "height"));

    Opal.cdecl($scope, 'Position', $scope.get('Struct').$new("x", "y"));
  })(self);
  (function($base, $super) {
    function $String(){};
    var self = $String = $klass($base, $super, 'String', $String);

    var def = self.$$proto, $scope = self.$$scope;

    def.$encode_uri_component = function() {
      var self = this;

      return encodeURIComponent(self);
    };

    def.$encode_uri = function() {
      var self = this;

      return encodeURI(self);
    };

    def.$decode_uri_component = function() {
      var self = this;

      return decodeURIComponent(self);
    };

    return (def.$decode_uri = function() {
      var self = this;

      return decodeURI(self);
    }, nil) && 'decode_uri';
  })(self, null);
  (function($base, $super) {
    function $Hash(){};
    var self = $Hash = $klass($base, $super, 'Hash', $Hash);

    var def = self.$$proto, $scope = self.$$scope;

    Opal.defs(self, '$decode_uri', function(string) {
      var $a, $b, TMP_1, self = this;

      return self['$[]'](($a = ($b = string.$split("&")).$map, $a.$$p = (TMP_1 = function(part){var self = TMP_1.$$s || this, $a, name = nil, value = nil;
if (part == null) part = nil;
      $a = Opal.to_ary(part.$split("=")), name = ($a[0] == null ? nil : $a[0]), value = ($a[1] == null ? nil : $a[1]);
        return [name.$decode_uri_component(), value.$decode_uri_component()];}, TMP_1.$$s = self, TMP_1), $a).call($b));
    });

    return (def.$encode_uri = function() {
      var $a, $b, TMP_2, self = this;

      return ($a = ($b = self).$map, $a.$$p = (TMP_2 = function(name, value){var self = TMP_2.$$s || this;
if (name == null) name = nil;if (value == null) value = nil;
      return "" + (name.$to_s().$encode_uri_component()) + "=" + (value.$to_s().$encode_uri_component())}, TMP_2.$$s = self, TMP_2), $a).call($b).$join("&");
    }, nil) && 'encode_uri';
  })(self, null);
  return (function($base, $super) {
    function $Object(){};
    var self = $Object = $klass($base, $super, 'Object', $Object);

    var def = self.$$proto, $scope = self.$$scope;

    return (Opal.defn(self, '$encode_uri', function() {
      var self = this;

      return self.$to_s().$encode_uri();
    }), nil) && 'encode_uri'
  })(self, null);
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/compatibility"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module;

  Opal.add_stubs(['$downcase', '$==', '$length']);
  Opal.cdecl($scope, 'BROWSER_ENGINE', (function() {try {return (/MSIE|WebKit|Presto|Gecko/.exec(navigator.userAgent)[0]).$downcase() } catch ($err) { return "unknown" }})());
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'Compatibility');

      var def = self.$$proto, $scope = self.$$scope;

      Opal.defs(self, '$sizzle?', function() {
        var self = this;

        return (typeof(window.Sizzle) !== "undefined");
      });

      Opal.defs(self, '$respond_to?', function(args) {
        var $a, self = this, parent = nil, object = nil, method = nil;

        args = $slice.call(arguments, 0);
        if (args.$length()['$=='](2)) {
          parent = window;
          $a = Opal.to_ary(args), object = ($a[0] == null ? nil : $a[0]), method = ($a[1] == null ? nil : $a[1]);
          } else {
          $a = Opal.to_ary(args), parent = ($a[0] == null ? nil : $a[0]), object = ($a[1] == null ? nil : $a[1]), method = ($a[2] == null ? nil : $a[2])
        };
        
      if (!parent) {
        return false;
      }

      var klass = parent[object];

      if (!klass) {
        return false;
      }

      return typeof(klass.prototype[method]) === "function";
    ;
      });

      Opal.defs(self, '$has?', function(args) {
        var $a, self = this, parent = nil, name = nil;

        args = $slice.call(arguments, 0);
        if (args.$length()['$=='](1)) {
          parent = window;
          $a = Opal.to_ary(args), name = ($a[0] == null ? nil : $a[0]);
          } else {
          $a = Opal.to_ary(args), parent = ($a[0] == null ? nil : $a[0]), name = ($a[1] == null ? nil : $a[1])
        };
        
      if (!parent) {
        return false;
      }

      return parent[name] != null;
    ;
      });
    })(self);

    Opal.cdecl($scope, 'C', $scope.get('Compatibility'));
  })(self);
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/interval"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass, $gvars = Opal.gvars;

  Opal.add_stubs(['$attr_reader', '$convert', '$start', '$aborted?', '$raise', '$stopped?', '$to_n', '$new', '$to_proc', '$every']);
  (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base, $super) {
      function $Interval(){};
      var self = $Interval = $klass($base, $super, 'Interval', $Interval);

      var def = self.$$proto, $scope = self.$$scope, TMP_1;

      def.stopped = def.aborted = def.window = def.id = def.block = def.every = nil;
      self.$attr_reader("every");

      def.$initialize = TMP_1 = function(window, time) {
        var self = this, $iter = TMP_1.$$p, block = $iter || nil;

        TMP_1.$$p = null;
        self.window = $scope.get('Native').$convert(window);
        self.every = time;
        self.block = block;
        self.aborted = false;
        self.stopped = true;
        return self.$start();
      };

      def['$stopped?'] = function() {
        var self = this;

        return self.stopped;
      };

      def['$aborted?'] = function() {
        var self = this;

        return self.aborted;
      };

      def.$abort = function() {
        var self = this;

        self.window.clearInterval(self.id);
        self.aborted = true;
        self.id = nil;
        return self;
      };

      def.$stop = function() {
        var self = this;

        self.window.clearInterval(self.id);
        self.stopped = true;
        return self.id = nil;
      };

      return (def.$start = function() {
        var $a, self = this;

        if ((($a = self['$aborted?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
          self.$raise("the interval has been aborted")};
        if ((($a = self['$stopped?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
          } else {
          return nil
        };
        self.id = self.window.setInterval(self.block.$to_n(), self.every * 1000);
        return self;
      }, nil) && 'start';
    })(self, null);

    (function($base, $super) {
      function $Window(){};
      var self = $Window = $klass($base, $super, 'Window', $Window);

      var def = self.$$proto, $scope = self.$$scope, TMP_2;

      def["native"] = nil;
      return (def.$every = TMP_2 = function(time) {
        var $a, $b, self = this, $iter = TMP_2.$$p, block = $iter || nil;

        TMP_2.$$p = null;
        return ($a = ($b = $scope.get('Interval')).$new, $a.$$p = block.$to_proc(), $a).call($b, self["native"], time);
      }, nil) && 'every'
    })(self, null);
  })(self);
  (function($base, $super) {
    function $Proc(){};
    var self = $Proc = $klass($base, $super, 'Proc', $Proc);

    var def = self.$$proto, $scope = self.$$scope;

    return (def.$every = function(time) {
      var $a, $b, self = this;
      if ($gvars.window == null) $gvars.window = nil;

      return ($a = ($b = $gvars.window).$every, $a.$$p = self.$to_proc(), $a).call($b, time);
    }, nil) && 'every'
  })(self, null);
  return (function($base) {
    var self = $module($base, 'Kernel');

    var def = self.$$proto, $scope = self.$$scope, TMP_3;

    Opal.defn(self, '$every', TMP_3 = function(time) {
      var $a, $b, self = this, $iter = TMP_3.$$p, block = $iter || nil;
      if ($gvars.window == null) $gvars.window = nil;

      TMP_3.$$p = null;
      return ($a = ($b = $gvars.window).$every, $a.$$p = block.$to_proc(), $a).call($b, time);
    })
  })(self);
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/timeout"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass, $gvars = Opal.gvars;

  Opal.add_stubs(['$attr_reader', '$convert', '$start', '$to_n', '$new', '$to_proc', '$after']);
  (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base, $super) {
      function $Timeout(){};
      var self = $Timeout = $klass($base, $super, 'Timeout', $Timeout);

      var def = self.$$proto, $scope = self.$$scope, TMP_1;

      def.window = def.id = def.block = def.after = nil;
      self.$attr_reader("after");

      def.$initialize = TMP_1 = function(window, time) {
        var self = this, $iter = TMP_1.$$p, block = $iter || nil;

        TMP_1.$$p = null;
        self.window = $scope.get('Native').$convert(window);
        self.after = time;
        self.block = block;
        return self.$start();
      };

      def.$abort = function() {
        var self = this;

        self.window.clearTimeout(self.id);
        return self;
      };

      return (def.$start = function() {
        var self = this;

        self.id = self.window.setTimeout(self.block.$to_n(), self.after * 1000);
        return self;
      }, nil) && 'start';
    })(self, null);

    (function($base, $super) {
      function $Window(){};
      var self = $Window = $klass($base, $super, 'Window', $Window);

      var def = self.$$proto, $scope = self.$$scope, TMP_2;

      def["native"] = nil;
      return (def.$after = TMP_2 = function(time) {
        var $a, $b, self = this, $iter = TMP_2.$$p, block = $iter || nil;

        TMP_2.$$p = null;
        return ($a = ($b = $scope.get('Timeout')).$new, $a.$$p = block.$to_proc(), $a).call($b, self["native"], time);
      }, nil) && 'after'
    })(self, null);
  })(self);
  (function($base, $super) {
    function $Proc(){};
    var self = $Proc = $klass($base, $super, 'Proc', $Proc);

    var def = self.$$proto, $scope = self.$$scope;

    return (def.$after = function(time) {
      var $a, $b, self = this;
      if ($gvars.window == null) $gvars.window = nil;

      return ($a = ($b = $gvars.window).$after, $a.$$p = self.$to_proc(), $a).call($b, time);
    }, nil) && 'after'
  })(self, null);
  return (function($base) {
    var self = $module($base, 'Kernel');

    var def = self.$$proto, $scope = self.$$scope, TMP_3;

    Opal.defn(self, '$after', TMP_3 = function(time) {
      var $a, $b, self = this, $iter = TMP_3.$$p, block = $iter || nil;
      if ($gvars.window == null) $gvars.window = nil;

      TMP_3.$$p = null;
      return ($a = ($b = $gvars.window).$after, $a.$$p = block.$to_proc(), $a).call($b, time);
    })
  })(self);
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/window/view"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass;

  Opal.add_stubs(['$to_n']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base, $super) {
      function $Window(){};
      var self = $Window = $klass($base, $super, 'Window', $Window);

      var def = self.$$proto, $scope = self.$$scope;

      return (function($base, $super) {
        function $View(){};
        var self = $View = $klass($base, $super, 'View', $View);

        var def = self.$$proto, $scope = self.$$scope;

        def["native"] = nil;
        def.$initialize = function(window) {
          var self = this;

          self.window = window;
          return self["native"] = window.$to_n();
        };

        def.$width = function() {
          var self = this;

          return self["native"].innerWidth;
        };

        return (def.$height = function() {
          var self = this;

          return self["native"].innerHeight;
        }, nil) && 'height';
      })(self, null)
    })(self, null)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/window/size"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass, $hash2 = Opal.hash2;

  Opal.add_stubs(['$to_n', '$[]', '$width', '$height', '$set']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base, $super) {
      function $Window(){};
      var self = $Window = $klass($base, $super, 'Window', $Window);

      var def = self.$$proto, $scope = self.$$scope;

      return (function($base, $super) {
        function $Size(){};
        var self = $Size = $klass($base, $super, 'Size', $Size);

        var def = self.$$proto, $scope = self.$$scope;

        def["native"] = nil;
        def.$initialize = function(window) {
          var self = this;

          self.window = window;
          return self["native"] = window.$to_n();
        };

        def.$set = function(what) {
          var $a, self = this, width = nil, height = nil;

          width = ((($a = what['$[]']("width")) !== false && $a !== nil) ? $a : self.$width());
          height = ((($a = what['$[]']("height")) !== false && $a !== nil) ? $a : self.$height());
          self["native"].resizeTo(width, height);
          return self;
        };

        def.$width = function() {
          var self = this;

          return self["native"].outerWidth;
        };

        def['$width='] = function(value) {
          var self = this;

          return self.$set($hash2(["width"], {"width": value}));
        };

        def.$height = function() {
          var self = this;

          return self["native"].outerHeight;
        };

        return (def['$height='] = function(value) {
          var self = this;

          return self.$set($hash2(["height"], {"height": value}));
        }, nil) && 'height=';
      })(self, null)
    })(self, null)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/window/scroll"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass;

  Opal.add_stubs(['$to_n', '$new', '$x', '$position', '$y', '$[]']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base, $super) {
      function $Window(){};
      var self = $Window = $klass($base, $super, 'Window', $Window);

      var def = self.$$proto, $scope = self.$$scope;

      return (function($base, $super) {
        function $Scroll(){};
        var self = $Scroll = $klass($base, $super, 'Scroll', $Scroll);

        var def = self.$$proto, $scope = self.$$scope;

        def["native"] = nil;
        def.$initialize = function(window) {
          var self = this;

          self.window = window;
          return self["native"] = window.$to_n();
        };

        def.$position = function() {
          var self = this;

          
      var doc  = self["native"].document,
          root = doc.documentElement,
          body = doc.body;

      var x = root.scrollLeft || body.scrollLeft,
          y = root.scrollTop  || body.scrollTop;
    ;
          return $scope.get('Position').$new(x, y);
        };

        def.$x = function() {
          var self = this;

          return self.$position().$x();
        };

        def.$y = function() {
          var self = this;

          return self.$position().$y();
        };

        def.$to = function(what) {
          var $a, self = this, x = nil, y = nil;

          x = ((($a = what['$[]']("x")) !== false && $a !== nil) ? $a : self.$x());
          y = ((($a = what['$[]']("y")) !== false && $a !== nil) ? $a : self.$y());
          self["native"].scrollTo(x, y);
          return self;
        };

        return (def.$by = function(what) {
          var $a, self = this, x = nil, y = nil;

          x = ((($a = what['$[]']("x")) !== false && $a !== nil) ? $a : 0);
          y = ((($a = what['$[]']("y")) !== false && $a !== nil) ? $a : 0);
          self["native"].scrollBy(x, y);
          return self;
        }, nil) && 'by';
      })(self, null)
    })(self, null)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/compatibility/window/view"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass;

  Opal.add_stubs(['$has?']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base, $super) {
      function $Window(){};
      var self = $Window = $klass($base, $super, 'Window', $Window);

      var def = self.$$proto, $scope = self.$$scope;

      return (function($base, $super) {
        function $View(){};
        var self = $View = $klass($base, $super, 'View', $View);

        var def = self.$$proto, $scope = self.$$scope, $a;

        def["native"] = nil;
        if ((($a = $scope.get('C')['$has?']("innerHeight")) !== nil && (!$a.$$is_boolean || $a == true))) {
          return nil
          } else {
          def.$width = function() {
            var self = this;

            return self["native"].document.documentElement.clientWidth;
          };

          return (def.$height = function() {
            var self = this;

            return self["native"].document.documentElement.clientHeight;
          }, nil) && 'height';
        }
      })(self, null)
    })(self, null)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/compatibility/window/size"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass;

  Opal.add_stubs(['$has?', '$raise']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base, $super) {
      function $Window(){};
      var self = $Window = $klass($base, $super, 'Window', $Window);

      var def = self.$$proto, $scope = self.$$scope;

      return (function($base, $super) {
        function $Size(){};
        var self = $Size = $klass($base, $super, 'Size', $Size);

        var def = self.$$proto, $scope = self.$$scope, $a;

        if ((($a = $scope.get('C')['$has?']("outerHeight")) !== nil && (!$a.$$is_boolean || $a == true))) {
          return nil
          } else {
          def.$width = function() {
            var self = this;

            return self.$raise($scope.get('NotImplementedError'), "window outer size not supported");
          };

          return (def.$height = function() {
            var self = this;

            return self.$raise($scope.get('NotImplementedError'), "window outer size not supported");
          }, nil) && 'height';
        }
      })(self, null)
    })(self, null)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/compatibility/window/scroll"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass;

  Opal.add_stubs(['$has?', '$new', '$x', '$y', '$raise']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base, $super) {
      function $Window(){};
      var self = $Window = $klass($base, $super, 'Window', $Window);

      var def = self.$$proto, $scope = self.$$scope;

      return (function($base, $super) {
        function $Scroll(){};
        var self = $Scroll = $klass($base, $super, 'Scroll', $Scroll);

        var def = self.$$proto, $scope = self.$$scope, $a;

        def["native"] = nil;
        if ((($a = $scope.get('C')['$has?'](document.documentElement, "scrollLeft")) !== nil && (!$a.$$is_boolean || $a == true))) {
          return nil
        } else if ((($a = $scope.get('C')['$has?']("pageXOffset")) !== nil && (!$a.$$is_boolean || $a == true))) {
          def.$position = function() {
            var self = this;

            return $scope.get('Position').$new(self.$x(), self.$y());
          };

          def.$x = function() {
            var self = this;

            return self["native"].pageXOffset;
          };

          return (def.$y = function() {
            var self = this;

            return self["native"].pageYOffset;
          }, nil) && 'y';
          } else {
          def.$x = function() {
            var self = this;

            return self.$raise($scope.get('NotImplementedError'), "window scroll unsupported");
          };

          return (def.$y = function() {
            var self = this;

            return self.$raise($scope.get('NotImplementedError'), "window scroll unsupported");
          }, nil) && 'y';
        }
      })(self, null)
    })(self, null)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/window/compatibility"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice;

  Opal.add_stubs(['$require']);
  self.$require("browser/compatibility/window/view");
  self.$require("browser/compatibility/window/size");
  return self.$require("browser/compatibility/window/scroll");
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/window"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass, $hash2 = Opal.hash2, $gvars = Opal.gvars;

  Opal.add_stubs(['$require', '$delete', '$join', '$map', '$===', '$new', '$include', '$[]', '$alert', '$prompt', '$confirm']);
  self.$require("browser/interval");
  self.$require("browser/timeout");
  self.$require("browser/window/view");
  self.$require("browser/window/size");
  self.$require("browser/window/scroll");
  self.$require("browser/window/compatibility");
  (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base, $super) {
      function $Window(){};
      var self = $Window = $klass($base, $super, 'Window', $Window);

      var def = self.$$proto, $scope = self.$$scope;

      def["native"] = nil;
      Opal.defs(self, '$open', function(url, options) {
        var $a, $b, TMP_1, self = this, name = nil, features = nil;

        name = options.$delete("name");
        features = ($a = ($b = options).$map, $a.$$p = (TMP_1 = function(key, value){var self = TMP_1.$$s || this, $case = nil;
if (key == null) key = nil;if (value == null) value = nil;
        value = (function() {$case = value;if (true['$===']($case)) {return "yes"}else if (false['$===']($case)) {return "no"}else {return value}})();
          return "" + (key) + "=" + (value);}, TMP_1.$$s = self, TMP_1), $a).call($b).$join(",");
        
      var win = window.open(url, name, features);

      if (win == null) {
        return nil;
      }

      return self.$new(win);
    ;
      });

      self.$include($scope.get('Native'));

      def.$alert = function(value) {
        var self = this;

        self["native"].alert(value);
        return value;
      };

      def.$prompt = function(value) {
        var self = this;

        return self["native"].prompt(value) || nil;
      };

      def.$confirm = function(value) {
        var self = this;

        return self["native"].confirm(value) || false;
      };

      def.$view = function() {
        var self = this;

        return $scope.get('View').$new(self);
      };

      def.$size = function() {
        var self = this;

        return $scope.get('Size').$new(self);
      };

      def.$scroll = function() {
        var self = this;

        return $scope.get('Scroll').$new(self);
      };

      def['$send!'] = function(message, options) {
        var $a, self = this;

        if (options == null) {
          options = $hash2([], {})
        }
        return self["native"].postMessage(message, ((($a = options['$[]']("to")) !== false && $a !== nil) ? $a : "*"));
      };

      return (def.$close = function() {
        var self = this;

        
      return (window.open('', '_self', '') && window.close()) ||
             (window.opener = null && window.close()) ||
             (window.opener = '' && window.close());
    
      }, nil) && 'close';
    })(self, null)
  })(self);
  $gvars.window = (($scope.get('Browser')).$$scope.get('Window')).$new(window);
  return (function($base) {
    var self = $module($base, 'Kernel');

    var def = self.$$proto, $scope = self.$$scope;

    Opal.defn(self, '$alert', function(value) {
      var self = this;
      if ($gvars.window == null) $gvars.window = nil;

      return $gvars.window.$alert(value);
    });

    Opal.defn(self, '$prompt', function(value) {
      var self = this;
      if ($gvars.window == null) $gvars.window = nil;

      return $gvars.window.$prompt(value);
    });

    Opal.defn(self, '$confirm', function(value) {
      var self = this;
      if ($gvars.window == null) $gvars.window = nil;

      return $gvars.window.$confirm(value);
    });
  })(self);
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/event/base"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass, $hash2 = Opal.hash2;

  Opal.add_stubs(['$include', '$call', '$to_n', '$<<', '$converters', '$native?', '$each', '$instance_eval', '$register', '$to_proc', '$attr_reader', '$new', '$stopped?', '$arguments', '$off', '$target', '$raise', '$name_for', '$push', '$callbacks', '$observe', '$deferred', '$on', '$css', '$===', '$delete', '$name', '$include?', '$gsub', '$delete_if', '$==', '$=~', '$clear', '$is_a?', '$create', '$private', '$defer', '$added', '$matches?', '$elements']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Event(){};
        var self = $Event = $klass($base, $super, 'Event', $Event);

        var def = self.$$proto, $scope = self.$$scope;

        self.$include($scope.get('Native'));

        (function($base, $super) {
          function $Definition(){};
          var self = $Definition = $klass($base, $super, 'Definition', $Definition);

          var def = self.$$proto, $scope = self.$$scope, TMP_1;

          def["native"] = nil;
          self.$include($scope.get('Native'));

          Opal.defs(self, '$new', TMP_1 = function() {
            var self = this, $iter = TMP_1.$$p, block = $iter || nil, data = nil;

            TMP_1.$$p = null;
            data = Opal.find_super_dispatcher(self, 'new', TMP_1, null, $Definition).apply(self, [{}]);
            if (block !== false && block !== nil) {
              block.$call(data)};
            return data.$to_n();
          });

          def['$bubbles!'] = function() {
            var self = this;

            return self["native"].bubbles = true;
          };

          return (def['$cancelable!'] = function() {
            var self = this;

            return self["native"].cancelable = true;
          }, nil) && 'cancelable!';
        })(self, null);

        return (function($base) {
          var self = $module($base, 'Target');

          var def = self.$$proto, $scope = self.$$scope, TMP_2, TMP_7, TMP_11;

          Opal.defs(self, '$converters', function() {
            var $a, self = this;
            if (self.converters == null) self.converters = nil;

            return ((($a = self.converters) !== false && $a !== nil) ? $a : self.converters = []);
          });

          Opal.defs(self, '$register', TMP_2 = function() {
            var self = this, $iter = TMP_2.$$p, block = $iter || nil;

            TMP_2.$$p = null;
            return self.$converters()['$<<'](block);
          });

          Opal.defs(self, '$convert', function(value) {try {

            var $a, $b, TMP_3, self = this;

            if ((($a = self['$native?'](value)) !== nil && (!$a.$$is_boolean || $a == true))) {
              } else {
              return value
            };
            ($a = ($b = self.$converters()).$each, $a.$$p = (TMP_3 = function(block){var self = TMP_3.$$s || this, $a, result = nil;
if (block == null) block = nil;
            if ((($a = result = block.$call(value)) !== nil && (!$a.$$is_boolean || $a == true))) {
                Opal.ret(result)
                } else {
                return nil
              }}, TMP_3.$$s = self, TMP_3), $a).call($b);
            return nil;
            } catch ($returner) { if ($returner === Opal.returner) { return $returner.$v } throw $returner; }
          });

          Opal.defs(self, '$included', function(klass) {
            var $a, $b, TMP_4, self = this;

            return ($a = ($b = klass).$instance_eval, $a.$$p = (TMP_4 = function(){var self = TMP_4.$$s || this, TMP_5;

            return (Opal.defs(self, '$target', TMP_5 = function() {
                var $a, $b, self = this, $iter = TMP_5.$$p, block = $iter || nil;

                TMP_5.$$p = null;
                return ($a = ($b = (((($scope.get('DOM')).$$scope.get('Event'))).$$scope.get('Target'))).$register, $a.$$p = block.$to_proc(), $a).call($b);
              }), nil) && 'target'}, TMP_4.$$s = self, TMP_4), $a).call($b);
          });

          (function($base, $super) {
            function $Callback(){};
            var self = $Callback = $klass($base, $super, 'Callback', $Callback);

            var def = self.$$proto, $scope = self.$$scope, TMP_6;

            def["function"] = nil;
            self.$attr_reader("target", "name", "selector");

            def.$initialize = TMP_6 = function(target, name, selector) {
              var $a, self = this, $iter = TMP_6.$$p, block = $iter || nil;

              if (selector == null) {
                selector = nil
              }
              TMP_6.$$p = null;
              
          callback = self;
          func     = function(event) {
            event = ((((Opal.get('Browser')).$$scope.get('DOM'))).$$scope.get('Event')).$new(event, callback);

            if (!(event)['$stopped?']()) {
              ($a = block).$call.apply($a, [event].concat((event).$arguments()));
            }

            return !(event)['$stopped?']();
          }
        ;
              self["function"] = func;
              self.target = target;
              self.name = name;
              return self.selector = selector;
            };

            def.$off = function() {
              var self = this;

              return self.$target().$off(self);
            };

            return (def.$to_n = function() {
              var self = this;

              return self["function"];
            }, nil) && 'to_n';
          })(self, null);

          Opal.defn(self, '$on', TMP_7 = function(name, selector) {
            var $a, $b, $c, self = this, $iter = TMP_7.$$p, block = $iter || nil, callback = nil;
            if (self["native"] == null) self["native"] = nil;

            if (selector == null) {
              selector = nil
            }
            TMP_7.$$p = null;
            if (block !== false && block !== nil) {
              } else {
              self.$raise($scope.get('ArgumentError'), "no block has been passed")
            };
            name = $scope.get('Event').$name_for(name);
            callback = ($a = ($b = $scope.get('Callback')).$new, $a.$$p = block.$to_proc(), $a).call($b, self, name, selector);
            self.$callbacks().$push(callback);
            if (selector !== false && selector !== nil) {
              self.$observe();
              self.$deferred()['$<<']([name, selector, block]);
              ($a = ($c = self.$css(selector)).$on, $a.$$p = block.$to_proc(), $a).call($c, name);
              } else {
              self["native"].addEventListener(name, callback.$to_n());
            };
            return callback;
          });

          Opal.defn(self, '$off', function(what) {
            var $a, $b, TMP_8, $c, TMP_9, $d, TMP_10, self = this, $case = nil;
            if (self["native"] == null) self["native"] = nil;

            if (what == null) {
              what = nil
            }
            return (function() {$case = what;if ($scope.get('Callback')['$===']($case)) {self.$callbacks().$delete(what);
            return self["native"].removeEventListener(what.$name(), what.$to_n(), false);}else if ($scope.get('String')['$===']($case)) {if ((($a = ((($b = what['$include?']("*")) !== false && $b !== nil) ? $b : what['$include?']("?"))) !== nil && (!$a.$$is_boolean || $a == true))) {
              return self.$off($scope.get('Regexp').$new(what.$gsub(/\*/, ".*?").$gsub(/\?/, ".")))
              } else {
              what = $scope.get('Event').$name_for(what);
              return ($a = ($b = self.$callbacks()).$delete_if, $a.$$p = (TMP_8 = function(callback){var self = TMP_8.$$s || this;
                if (self["native"] == null) self["native"] = nil;
if (callback == null) callback = nil;
              if (callback.$name()['$=='](what)) {
                  self["native"].removeEventListener(callback.$name(), callback.$to_n(), false);
                  return true;
                  } else {
                  return nil
                }}, TMP_8.$$s = self, TMP_8), $a).call($b);
            }}else if ($scope.get('Regexp')['$===']($case)) {return ($a = ($c = self.$callbacks()).$delete_if, $a.$$p = (TMP_9 = function(callback){var self = TMP_9.$$s || this, $a;
              if (self["native"] == null) self["native"] = nil;
if (callback == null) callback = nil;
            if ((($a = callback.$name()['$=~'](what)) !== nil && (!$a.$$is_boolean || $a == true))) {
                self["native"].removeEventListener(callback.$name(), callback.$to_n(), false);
                return true;
                } else {
                return nil
              }}, TMP_9.$$s = self, TMP_9), $a).call($c)}else {($a = ($d = self.$callbacks()).$each, $a.$$p = (TMP_10 = function(callback){var self = TMP_10.$$s || this;
              if (self["native"] == null) self["native"] = nil;
if (callback == null) callback = nil;
            return self["native"].removeEventListener(callback.$name(), callback.$to_n(), false);}, TMP_10.$$s = self, TMP_10), $a).call($d);
            return self.$callbacks().$clear();}})();
          });

          Opal.defn(self, '$trigger', TMP_11 = function(event, args) {
            var $a, $b, self = this, $iter = TMP_11.$$p, block = $iter || nil;
            if (self["native"] == null) self["native"] = nil;

            args = $slice.call(arguments, 1);
            TMP_11.$$p = null;
            if ((($a = event['$is_a?']($scope.get('String'))) !== nil && (!$a.$$is_boolean || $a == true))) {
              event = ($a = ($b = $scope.get('Event')).$create, $a.$$p = block.$to_proc(), $a).apply($b, [event].concat(args))};
            return self["native"].dispatchEvent(event.$to_n());
          });

          self.$private();

          Opal.defn(self, '$callbacks', function() {
            var self = this;
            if (self["native"] == null) self["native"] = nil;

            
        if (!self["native"].$callbacks) {
          self["native"].$callbacks = [];
        }

        return self["native"].$callbacks;
      ;
          });

          Opal.defn(self, '$observe', function() {
            var $a, $b, TMP_12, self = this;
            if (self["native"] == null) self["native"] = nil;

            
        if (!self["native"].$observer) {
          self["native"].$observer = ($a = ($b = $scope.get('MutationObserver')).$new, $a.$$p = (TMP_12 = function(mutations){var self = TMP_12.$$s || this, $a, $b, TMP_13;
if (mutations == null) mutations = nil;
            return ($a = ($b = mutations).$each, $a.$$p = (TMP_13 = function(mutation){var self = TMP_13.$$s || this, $a, $b, TMP_14;
if (mutation == null) mutation = nil;
              return ($a = ($b = mutation.$added()).$each, $a.$$p = (TMP_14 = function(node){var self = TMP_14.$$s || this, $a;
if (node == null) node = nil;
                if ((($a = $scope.get('Element')['$==='](node)) !== nil && (!$a.$$is_boolean || $a == true))) {
                    } else {
                    return nil;
                  };
                  return self.$defer(node);}, TMP_14.$$s = self, TMP_14), $a).call($b)}, TMP_13.$$s = self, TMP_13), $a).call($b)}, TMP_12.$$s = self, TMP_12), $a).call($b);

          (self["native"].$observer).$observe(self["native"], $hash2(["children", "tree"], {"children": true, "tree": true}))
        }
      ;
          });

          Opal.defn(self, '$deferred', function() {
            var self = this;
            if (self["native"] == null) self["native"] = nil;

            
        if (!self["native"].$deferred) {
          self["native"].$deferred = [];
        }

        return self["native"].$deferred;
      ;
          });

          Opal.defn(self, '$defer', function(node) {
            var $a, $b, TMP_15, self = this;

            return ($a = ($b = self.$deferred()).$each, $a.$$p = (TMP_15 = function(name, selector, block){var self = TMP_15.$$s || this, $a, $b, $c, TMP_16;
if (name == null) name = nil;if (selector == null) selector = nil;if (block == null) block = nil;
            if ((($a = node['$matches?'](selector)) !== nil && (!$a.$$is_boolean || $a == true))) {
                ($a = ($b = node).$on, $a.$$p = block.$to_proc(), $a).call($b, name)};
              return ($a = ($c = node.$elements()).$each, $a.$$p = (TMP_16 = function(el){var self = TMP_16.$$s || this;
if (el == null) el = nil;
              return self.$defer(el)}, TMP_16.$$s = self, TMP_16), $a).call($c);}, TMP_15.$$s = self, TMP_15), $a).call($b);
          });
        })(self);
      })(self, null)
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/event/ui"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass;

  Opal.add_stubs(['$new', '$to_proc', '$alias_native']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Event(){};
        var self = $Event = $klass($base, $super, 'Event', $Event);

        var def = self.$$proto, $scope = self.$$scope;

        return (function($base, $super) {
          function $UI(){};
          var self = $UI = $klass($base, $super, 'UI', $UI);

          var def = self.$$proto, $scope = self.$$scope, TMP_1;

          (function($base, $super) {
            function $Definition(){};
            var self = $Definition = $klass($base, $super, 'Definition', $Definition);

            var def = self.$$proto, $scope = self.$$scope;

            def["native"] = nil;
            def['$detail='] = function(value) {
              var self = this;

              return self["native"].detail = value;
            };

            return (def['$view='] = function(value) {
              var self = this;

              return self["native"].view = value;
            }, nil) && 'view=';
          })(self, $scope.get('Definition'));

          Opal.defs(self, '$create', TMP_1 = function(name) {
            var $a, $b, self = this, $iter = TMP_1.$$p, block = $iter || nil;

            TMP_1.$$p = null;
            return self.$new(new UIEvent(name, ($a = ($b = $scope.get('Definition')).$new, $a.$$p = block.$to_proc(), $a).call($b)));
          });

          self.$alias_native("detail");

          return self.$alias_native("view");
        })(self, $scope.get('Event'))
      })(self, null)
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/event/mouse"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass, $gvars = Opal.gvars;

  Opal.add_stubs(['$!', '$nil?', '$[]', '$include', '$new', '$try_convert', '$to_proc', '$x', '$screen', '$y', '$DOM', '$==', '$downcase', '$name']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Event(){};
        var self = $Event = $klass($base, $super, 'Event', $Event);

        var def = self.$$proto, $scope = self.$$scope;

        return (function($base, $super) {
          function $Mouse(){};
          var self = $Mouse = $klass($base, $super, 'Mouse', $Mouse);

          var def = self.$$proto, $scope = self.$$scope, TMP_1;

          def["native"] = nil;
          Opal.defs(self, '$supported?', function() {
            var self = this;
            if ($gvars.$ == null) $gvars.$ = nil;

            return $gvars.$['$[]']("MouseEvent")['$nil?']()['$!']();
          });

          (function($base, $super) {
            function $Definition(){};
            var self = $Definition = $klass($base, $super, 'Definition', $Definition);

            var def = self.$$proto, $scope = self.$$scope;

            def["native"] = nil;
            (function($base, $super) {
              function $Client(){};
              var self = $Client = $klass($base, $super, 'Client', $Client);

              var def = self.$$proto, $scope = self.$$scope;

              def["native"] = nil;
              self.$include($scope.get('Native'));

              def['$x='] = function(value) {
                var self = this;

                return self["native"].clientX = value;
              };

              return (def['$y='] = function(value) {
                var self = this;

                return self["native"].clientY = value;
              }, nil) && 'y=';
            })(self, null);

            (function($base, $super) {
              function $Layer(){};
              var self = $Layer = $klass($base, $super, 'Layer', $Layer);

              var def = self.$$proto, $scope = self.$$scope;

              def["native"] = nil;
              self.$include($scope.get('Native'));

              def['$x='] = function(value) {
                var self = this;

                return self["native"].layerX = value;
              };

              return (def['$y='] = function(value) {
                var self = this;

                return self["native"].layerY = value;
              }, nil) && 'y=';
            })(self, null);

            (function($base, $super) {
              function $Offset(){};
              var self = $Offset = $klass($base, $super, 'Offset', $Offset);

              var def = self.$$proto, $scope = self.$$scope;

              def["native"] = nil;
              self.$include($scope.get('Native'));

              def['$x='] = function(value) {
                var self = this;

                return self["native"].offsetX = value;
              };

              return (def['$y='] = function(value) {
                var self = this;

                return self["native"].offsetY= value;
              }, nil) && 'y=';
            })(self, null);

            (function($base, $super) {
              function $Page(){};
              var self = $Page = $klass($base, $super, 'Page', $Page);

              var def = self.$$proto, $scope = self.$$scope;

              def["native"] = nil;
              self.$include($scope.get('Native'));

              def['$x='] = function(value) {
                var self = this;

                return self["native"].pageX = value;
              };

              return (def['$y='] = function(value) {
                var self = this;

                return self["native"].pageY = value;
              }, nil) && 'y=';
            })(self, null);

            (function($base, $super) {
              function $Screen(){};
              var self = $Screen = $klass($base, $super, 'Screen', $Screen);

              var def = self.$$proto, $scope = self.$$scope;

              def["native"] = nil;
              self.$include($scope.get('Native'));

              def['$x='] = function(value) {
                var self = this;

                return self["native"].screenX = value;
              };

              return (def['$y='] = function(value) {
                var self = this;

                return self["native"].screenY = value;
              }, nil) && 'y=';
            })(self, null);

            (function($base, $super) {
              function $Ancestor(){};
              var self = $Ancestor = $klass($base, $super, 'Ancestor', $Ancestor);

              var def = self.$$proto, $scope = self.$$scope;

              def["native"] = nil;
              self.$include($scope.get('Native'));

              def['$x='] = function(value) {
                var self = this;

                return self["native"].x = value;
              };

              return (def['$y='] = function(value) {
                var self = this;

                return self["native"].y = value;
              }, nil) && 'y=';
            })(self, null);

            def['$x='] = function(value) {
              var self = this;

              return self["native"].screenX = value;
            };

            def['$y='] = function(value) {
              var self = this;

              return self["native"].screenY = value;
            };

            def['$alt!'] = function() {
              var self = this;

              return self["native"].altKey = true;
            };

            def['$ctrl!'] = function() {
              var self = this;

              return self["native"].ctrlKey = true;
            };

            def['$meta!'] = function() {
              var self = this;

              return self["native"].metaKey = true;
            };

            def['$button='] = function(value) {
              var self = this;

              return self["native"].button = value;
            };

            def.$client = function() {
              var self = this;

              return $scope.get('Client').$new(self["native"]);
            };

            def.$layer = function() {
              var self = this;

              return $scope.get('Layer').$new(self["native"]);
            };

            def.$offset = function() {
              var self = this;

              return $scope.get('Offset').$new(self["native"]);
            };

            def.$page = function() {
              var self = this;

              return $scope.get('Page').$new(self["native"]);
            };

            def.$screen = function() {
              var self = this;

              return $scope.get('Screen').$new(self["native"]);
            };

            def.$ancestor = function() {
              var self = this;

              return $scope.get('Ancestor').$new(self["native"]);
            };

            def['$related='] = function(elem) {
              var self = this;

              return self["native"].relatedTarget = $scope.get('Native').$try_convert(elem);
            };

            def['$from='] = function(elem) {
              var self = this;

              return self["native"].fromElement = $scope.get('Native').$try_convert(elem);
            };

            return (def['$to='] = function(elem) {
              var self = this;

              return self["native"].toElement = $scope.get('Native').$try_convert(elem);
            }, nil) && 'to=';
          })(self, (($scope.get('UI')).$$scope.get('Definition')));

          Opal.defs(self, '$create', TMP_1 = function(name) {
            var $a, $b, self = this, $iter = TMP_1.$$p, block = $iter || nil;

            TMP_1.$$p = null;
            return self.$new(new MouseEvent(name, ($a = ($b = $scope.get('Definition')).$new, $a.$$p = block.$to_proc(), $a).call($b)));
          });

          Opal.cdecl($scope, 'Position', $scope.get('Struct').$new("x", "y"));

          def['$alt?'] = function() {
            var self = this;

            return self["native"].altKey;
          };

          def['$ctrl?'] = function() {
            var self = this;

            return self["native"].ctrlKey;
          };

          def['$meta?'] = function() {
            var self = this;

            return self["native"].metaKey;
          };

          def['$shift?'] = function() {
            var self = this;

            return self["native"].shiftKey;
          };

          def.$button = function() {
            var self = this;

            return self["native"].button;
          };

          def.$client = function() {
            var self = this;

            return $scope.get('Position').$new(self["native"].clientX, self["native"].clientY);
          };

          def.$layer = function() {
            var $a, self = this;

            if ((($a = self["native"].layerX == null) !== nil && (!$a.$$is_boolean || $a == true))) {
              return nil
              } else {
              return $scope.get('Position').$new(self["native"].layerX, self["native"].layerY)
            };
          };

          def.$offset = function() {
            var $a, self = this;

            if ((($a = self["native"].offsetX == null) !== nil && (!$a.$$is_boolean || $a == true))) {
              return nil
              } else {
              return $scope.get('Position').$new(self["native"].offsetX, self["native"].offsetY)
            };
          };

          def.$page = function() {
            var $a, self = this;

            if ((($a = self["native"].pageX == null) !== nil && (!$a.$$is_boolean || $a == true))) {
              return nil
              } else {
              return $scope.get('Position').$new(self["native"].pageX, self["native"].pageY)
            };
          };

          def.$screen = function() {
            var $a, self = this;

            if ((($a = self["native"].screenX == null) !== nil && (!$a.$$is_boolean || $a == true))) {
              return nil
              } else {
              return $scope.get('Position').$new(self["native"].screenX, self["native"].screenY)
            };
          };

          def.$ancestor = function() {
            var $a, self = this;

            if ((($a = self["native"].x == null) !== nil && (!$a.$$is_boolean || $a == true))) {
              return nil
              } else {
              return $scope.get('Position').$new(self["native"].x, self["native"].y)
            };
          };

          def.$x = function() {
            var self = this;

            return self.$screen().$x();
          };

          def.$y = function() {
            var self = this;

            return self.$screen().$y();
          };

          def.$related = function() {
            var $a, self = this;

            if ((($a = self["native"].relatedTarget == null) !== nil && (!$a.$$is_boolean || $a == true))) {
              return nil
              } else {
              return self.$DOM(self["native"].relatedTarget)
            };
          };

          def.$from = function() {
            var $a, self = this;

            if ((($a = self["native"].fromElement == null) !== nil && (!$a.$$is_boolean || $a == true))) {
              return nil
              } else {
              return self.$DOM(self["native"].fromElement)
            };
          };

          def.$to = function() {
            var $a, self = this;

            if ((($a = self["native"].toElement == null) !== nil && (!$a.$$is_boolean || $a == true))) {
              return nil
              } else {
              return self.$DOM(self["native"].toElement)
            };
          };

          def['$click?'] = function() {
            var self = this;

            return self.$name().$downcase()['$==']("click");
          };

          def['$double_click?'] = function() {
            var self = this;

            return self.$name().$downcase()['$==']("dblclick");
          };

          def['$down?'] = function() {
            var self = this;

            return self.$name().$downcase()['$==']("mousedown");
          };

          def['$enter?'] = function() {
            var self = this;

            return self.$name().$downcase()['$==']("mouseenter");
          };

          def['$leave?'] = function() {
            var self = this;

            return self.$name().$downcase()['$==']("mouseleave");
          };

          def['$move?'] = function() {
            var self = this;

            return self.$name().$downcase()['$==']("mousemove");
          };

          def['$out?'] = function() {
            var self = this;

            return self.$name().$downcase()['$==']("mouseout");
          };

          def['$over?'] = function() {
            var self = this;

            return self.$name().$downcase()['$==']("mouseover");
          };

          def['$up?'] = function() {
            var self = this;

            return self.$name().$downcase()['$==']("mouseup");
          };

          return (def['$show?'] = function() {
            var self = this;

            return self.$name().$downcase()['$==']("show");
          }, nil) && 'show?';
        })(self, $scope.get('UI'))
      })(self, null)
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/event/keyboard"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass, $gvars = Opal.gvars;

  Opal.add_stubs(['$!', '$nil?', '$[]', '$new', '$to_proc', '$code', '$chr', '$==', '$downcase', '$name']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Event(){};
        var self = $Event = $klass($base, $super, 'Event', $Event);

        var def = self.$$proto, $scope = self.$$scope;

        return (function($base, $super) {
          function $Keyboard(){};
          var self = $Keyboard = $klass($base, $super, 'Keyboard', $Keyboard);

          var def = self.$$proto, $scope = self.$$scope, TMP_1;

          def["native"] = nil;
          Opal.defs(self, '$supported?', function() {
            var self = this;
            if ($gvars.$ == null) $gvars.$ = nil;

            return $gvars.$['$[]']("KeyboardEvent")['$nil?']()['$!']();
          });

          (function($base, $super) {
            function $Definition(){};
            var self = $Definition = $klass($base, $super, 'Definition', $Definition);

            var def = self.$$proto, $scope = self.$$scope;

            def["native"] = nil;
            def['$alt!'] = function() {
              var self = this;

              return self["native"].altKey = true;
            };

            def['$ctrl!'] = function() {
              var self = this;

              return self["native"].ctrlKey = true;
            };

            def['$meta!'] = function() {
              var self = this;

              return self["native"].metaKey = true;
            };

            def['$shift!'] = function() {
              var self = this;

              return self["native"].shiftKey = true;
            };

            def['$code='] = function(code) {
              var self = this;

              return self["native"].keyCode = self["native"].which = code;
            };

            def['$key='] = function(key) {
              var self = this;

              return self["native"].key = key;
            };

            def['$char='] = function(char$) {
              var self = this;

              return self["native"].char = self["native"].charCode = char$;
            };

            return (def['$repeat!'] = function() {
              var self = this;

              return self["native"].repeat = true;
            }, nil) && 'repeat!';
          })(self, (($scope.get('UI')).$$scope.get('Definition')));

          Opal.defs(self, '$create', TMP_1 = function(name) {
            var $a, $b, self = this, $iter = TMP_1.$$p, block = $iter || nil;

            TMP_1.$$p = null;
            return self.$new(new KeyboardEvent(name, ($a = ($b = $scope.get('Definition')).$new, $a.$$p = block.$to_proc(), $a).call($b)));
          });

          def['$alt?'] = function() {
            var self = this;

            return self["native"].altKey;
          };

          def['$ctrl?'] = function() {
            var self = this;

            return self["native"].ctrlKey;
          };

          def['$meta?'] = function() {
            var self = this;

            return self["native"].metaKey;
          };

          def['$shift?'] = function() {
            var self = this;

            return self["native"].shiftKey;
          };

          def['$repeat?'] = function() {
            var self = this;

            return self["native"].repeat;
          };

          def.$key = function() {
            var self = this;

            return self["native"].key || self["native"].keyIdentifier || nil;
          };

          def.$code = function() {
            var self = this;

            return self["native"].keyCode || self["native"].which || nil;
          };

          def.$char = function() {
            var $a, self = this;

            return self["native"].char || self["native"].charCode || (function() {if ((($a = self.$code()) !== nil && (!$a.$$is_boolean || $a == true))) {
              return self.$code().$chr()
              } else {
              return nil
            }; return nil; })();
          };

          Opal.defn(self, '$to_i', def.$key);

          def['$down?'] = function() {
            var self = this;

            return self.$name().$downcase()['$==']("keydown");
          };

          def['$press?'] = function() {
            var self = this;

            return self.$name().$downcase()['$==']("keypress");
          };

          return (def['$up?'] = function() {
            var self = this;

            return self.$name().$downcase()['$==']("keyup");
          }, nil) && 'up?';
        })(self, $scope.get('UI'))
      })(self, null)
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/event/focus"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass, $gvars = Opal.gvars;

  Opal.add_stubs(['$!', '$nil?', '$[]', '$try_convert', '$new', '$to_proc', '$DOM']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Event(){};
        var self = $Event = $klass($base, $super, 'Event', $Event);

        var def = self.$$proto, $scope = self.$$scope;

        return (function($base, $super) {
          function $Focus(){};
          var self = $Focus = $klass($base, $super, 'Focus', $Focus);

          var def = self.$$proto, $scope = self.$$scope, TMP_1;

          def["native"] = nil;
          Opal.defs(self, '$supported?', function() {
            var self = this;
            if ($gvars.$ == null) $gvars.$ = nil;

            return $gvars.$['$[]']("FocusEvent")['$nil?']()['$!']();
          });

          (function($base, $super) {
            function $Definition(){};
            var self = $Definition = $klass($base, $super, 'Definition', $Definition);

            var def = self.$$proto, $scope = self.$$scope;

            def["native"] = nil;
            return (def['$related='] = function(elem) {
              var self = this;

              return self["native"].relatedTarget = $scope.get('Native').$try_convert(elem);
            }, nil) && 'related='
          })(self, (($scope.get('UI')).$$scope.get('Definition')));

          Opal.defs(self, '$create', TMP_1 = function(name) {
            var $a, $b, self = this, $iter = TMP_1.$$p, block = $iter || nil;

            TMP_1.$$p = null;
            return self.$new(new FocusEvent(name, ($a = ($b = $scope.get('Definition')).$new, $a.$$p = block.$to_proc(), $a).call($b)));
          });

          return (def.$related = function() {
            var self = this;

            return self.$DOM(self["native"].relatedTarget);
          }, nil) && 'related';
        })(self, $scope.get('UI'))
      })(self, null)
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/event/wheel"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass, $gvars = Opal.gvars;

  Opal.add_stubs(['$!', '$nil?', '$[]', '$===', '$new', '$to_proc', '$alias_native']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Event(){};
        var self = $Event = $klass($base, $super, 'Event', $Event);

        var def = self.$$proto, $scope = self.$$scope;

        return (function($base, $super) {
          function $Wheel(){};
          var self = $Wheel = $klass($base, $super, 'Wheel', $Wheel);

          var def = self.$$proto, $scope = self.$$scope, TMP_1;

          def["native"] = nil;
          Opal.defs(self, '$supported?', function() {
            var self = this;
            if ($gvars.$ == null) $gvars.$ = nil;

            return $gvars.$['$[]']("WheelEvent")['$nil?']()['$!']();
          });

          (function($base, $super) {
            function $Definition(){};
            var self = $Definition = $klass($base, $super, 'Definition', $Definition);

            var def = self.$$proto, $scope = self.$$scope;

            def["native"] = nil;
            def['$x='] = function(value) {
              var self = this;

              return self["native"].deltaX = value;
            };

            def['$y='] = function(value) {
              var self = this;

              return self["native"].deltaY = value;
            };

            def['$z='] = function(value) {
              var self = this;

              return self["native"].deltaZ = value;
            };

            return (def['$mode='] = function(value) {
              var self = this, $case = nil;

              value = (function() {$case = value;if ("pixel"['$===']($case)) {return WheelEvent.DOM_DELTA_PIXEL;}else if ("line"['$===']($case)) {return WheelEvent.DOM_DELTA_LINE;}else if ("page"['$===']($case)) {return WheelEvent.DOM_DELTA_PAGE;}else { return nil }})();
              return self["native"].deltaMode = value;
            }, nil) && 'mode=';
          })(self, $scope.get('Definition'));

          Opal.defs(self, '$create', TMP_1 = function(name) {
            var $a, $b, self = this, $iter = TMP_1.$$p, block = $iter || nil;

            TMP_1.$$p = null;
            return self.$new(new WheelEvent(name, ($a = ($b = $scope.get('Definition')).$new, $a.$$p = block.$to_proc(), $a).call($b)));
          });

          self.$alias_native("x", "deltaX");

          self.$alias_native("y", "deltaY");

          self.$alias_native("z", "deltaZ");

          return (def.$mode = function() {
            var self = this, $case = nil;

            return (function() {$case = self["native"].deltaMode;if ((WheelEvent.DOM_DELTA_PIXEL)['$===']($case)) {return "pixel"}else if ((WheelEvent.DOM_DELTA_LINE)['$===']($case)) {return "line"}else if ((WheelEvent.DOM_DELTA_PAGE)['$===']($case)) {return "page"}else { return nil }})();
          }, nil) && 'mode';
        })(self, $scope.get('UI'))
      })(self, null)
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/event/composition"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass, $gvars = Opal.gvars;

  Opal.add_stubs(['$!', '$nil?', '$[]', '$new', '$to_proc', '$alias_native', '$==', '$downcase', '$name']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Event(){};
        var self = $Event = $klass($base, $super, 'Event', $Event);

        var def = self.$$proto, $scope = self.$$scope;

        return (function($base, $super) {
          function $Composition(){};
          var self = $Composition = $klass($base, $super, 'Composition', $Composition);

          var def = self.$$proto, $scope = self.$$scope, TMP_1;

          Opal.defs(self, '$supported?', function() {
            var self = this;
            if ($gvars.$ == null) $gvars.$ = nil;

            return $gvars.$['$[]']("CompositionEvent")['$nil?']()['$!']();
          });

          (function($base, $super) {
            function $Definition(){};
            var self = $Definition = $klass($base, $super, 'Definition', $Definition);

            var def = self.$$proto, $scope = self.$$scope;

            def["native"] = nil;
            def['$data='] = function(value) {
              var self = this;

              return self["native"].data = value;
            };

            return (def['$locale='] = function(value) {
              var self = this;

              return self["native"].locale = value;
            }, nil) && 'locale=';
          })(self, (($scope.get('UI')).$$scope.get('Definition')));

          Opal.defs(self, '$create', TMP_1 = function(name) {
            var $a, $b, self = this, $iter = TMP_1.$$p, block = $iter || nil;

            TMP_1.$$p = null;
            return self.$new(new CompositionEvent(name, ($a = ($b = $scope.get('Definition')).$new, $a.$$p = block.$to_proc(), $a).call($b)));
          });

          self.$alias_native("data");

          self.$alias_native("locale");

          def['$start?'] = function() {
            var self = this;

            return self.$name().$downcase()['$==']("compositionstart");
          };

          def['$update?'] = function() {
            var self = this;

            return self.$name().$downcase()['$==']("compositionupdate");
          };

          return (def['$end?'] = function() {
            var self = this;

            return self.$name().$downcase()['$==']("compositionend");
          }, nil) && 'end?';
        })(self, $scope.get('UI'))
      })(self, null)
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/event/animation"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass, $gvars = Opal.gvars;

  Opal.add_stubs(['$!', '$nil?', '$[]', '$new', '$to_proc', '$alias_native']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Event(){};
        var self = $Event = $klass($base, $super, 'Event', $Event);

        var def = self.$$proto, $scope = self.$$scope;

        return (function($base, $super) {
          function $Animation(){};
          var self = $Animation = $klass($base, $super, 'Animation', $Animation);

          var def = self.$$proto, $scope = self.$$scope, TMP_1;

          Opal.defs(self, '$supported?', function() {
            var self = this;
            if ($gvars.$ == null) $gvars.$ = nil;

            return $gvars.$['$[]']("AnimationEvent")['$nil?']()['$!']();
          });

          (function($base, $super) {
            function $Definition(){};
            var self = $Definition = $klass($base, $super, 'Definition', $Definition);

            var def = self.$$proto, $scope = self.$$scope;

            def["native"] = nil;
            def['$animation='] = function(value) {
              var self = this;

              return self["native"].animationName = value;
            };

            return (def['$elapsed='] = function(value) {
              var self = this;

              return self["native"].elapsedTime = value;
            }, nil) && 'elapsed=';
          })(self, $scope.get('Definition'));

          Opal.defs(self, '$create', TMP_1 = function(name) {
            var $a, $b, self = this, $iter = TMP_1.$$p, block = $iter || nil;

            TMP_1.$$p = null;
            return self.$new(new AnimationEvent(name, ($a = ($b = $scope.get('Definition')).$new, $a.$$p = block.$to_proc(), $a).call($b)));
          });

          self.$alias_native("name", "animationName");

          return self.$alias_native("elapsed", "elapsedTime");
        })(self, $scope.get('Event'))
      })(self, null)
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/event/audio_processing"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass, $gvars = Opal.gvars;

  Opal.add_stubs(['$!', '$nil?', '$[]', '$new', '$to_proc', '$alias_native']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Event(){};
        var self = $Event = $klass($base, $super, 'Event', $Event);

        var def = self.$$proto, $scope = self.$$scope;

        return (function($base, $super) {
          function $AudioProcessing(){};
          var self = $AudioProcessing = $klass($base, $super, 'AudioProcessing', $AudioProcessing);

          var def = self.$$proto, $scope = self.$$scope, TMP_1;

          Opal.defs(self, '$supported?', function() {
            var self = this;
            if ($gvars.$ == null) $gvars.$ = nil;

            return $gvars.$['$[]']("AudioProcessingEvent")['$nil?']()['$!']();
          });

          (function($base, $super) {
            function $Definition(){};
            var self = $Definition = $klass($base, $super, 'Definition', $Definition);

            var def = self.$$proto, $scope = self.$$scope;

            def["native"] = nil;
            def['$time='] = function(value) {
              var self = this;

              return self["native"].playbackTime = value;
            };

            def['$input='] = function(value) {
              var self = this;

              return self["native"].inputBuffer = value;
            };

            return (def['$output='] = function(value) {
              var self = this;

              return self["native"].outputBuffer = value;
            }, nil) && 'output=';
          })(self, $scope.get('Definition'));

          Opal.defs(self, '$create', TMP_1 = function(name) {
            var $a, $b, self = this, $iter = TMP_1.$$p, block = $iter || nil;

            TMP_1.$$p = null;
            return self.$new(new AudioProcessingEvent(name, ($a = ($b = $scope.get('Definition')).$new, $a.$$p = block.$to_proc(), $a).call($b)));
          });

          self.$alias_native("time", "playbackTime");

          self.$alias_native("input", "inputBuffer");

          return self.$alias_native("output", "outputBuffer");
        })(self, $scope.get('Event'))
      })(self, null)
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/event/before_unload"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass, $gvars = Opal.gvars;

  Opal.add_stubs(['$!', '$nil?', '$[]', '$new', '$to_proc']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Event(){};
        var self = $Event = $klass($base, $super, 'Event', $Event);

        var def = self.$$proto, $scope = self.$$scope;

        return (function($base, $super) {
          function $BeforeUnload(){};
          var self = $BeforeUnload = $klass($base, $super, 'BeforeUnload', $BeforeUnload);

          var def = self.$$proto, $scope = self.$$scope, TMP_1;

          Opal.defs(self, '$supported?', function() {
            var self = this;
            if ($gvars.$ == null) $gvars.$ = nil;

            return $gvars.$['$[]']("BeforeUnloadEvent")['$nil?']()['$!']();
          });

          return (Opal.defs(self, '$create', TMP_1 = function(name) {
            var $a, $b, self = this, $iter = TMP_1.$$p, block = $iter || nil;

            TMP_1.$$p = null;
            return self.$new(new BeforeUnloadEvent(name, ($a = ($b = $scope.get('Definition')).$new, $a.$$p = block.$to_proc(), $a).call($b)));
          }), nil) && 'create';
        })(self, $scope.get('Event'))
      })(self, null)
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/event/clipboard"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass, $gvars = Opal.gvars;

  Opal.add_stubs(['$!', '$nil?', '$[]', '$new', '$to_proc', '$alias_native']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Event(){};
        var self = $Event = $klass($base, $super, 'Event', $Event);

        var def = self.$$proto, $scope = self.$$scope;

        return (function($base, $super) {
          function $Clipboard(){};
          var self = $Clipboard = $klass($base, $super, 'Clipboard', $Clipboard);

          var def = self.$$proto, $scope = self.$$scope, TMP_1;

          Opal.defs(self, '$supported?', function() {
            var self = this;
            if ($gvars.$ == null) $gvars.$ = nil;

            return $gvars.$['$[]']("ClipboardEvent")['$nil?']()['$!']();
          });

          (function($base, $super) {
            function $Definition(){};
            var self = $Definition = $klass($base, $super, 'Definition', $Definition);

            var def = self.$$proto, $scope = self.$$scope;

            def["native"] = nil;
            def['$data='] = function(value) {
              var self = this;

              return self["native"].data = value;
            };

            return (def['$type='] = function(value) {
              var self = this;

              return self["native"].dataType = value;
            }, nil) && 'type=';
          })(self, $scope.get('Definition'));

          Opal.defs(self, '$create', TMP_1 = function(name) {
            var $a, $b, self = this, $iter = TMP_1.$$p, block = $iter || nil;

            TMP_1.$$p = null;
            return self.$new(new ClipboardEvent(name, ($a = ($b = $scope.get('Definition')).$new, $a.$$p = block.$to_proc(), $a).call($b)));
          });

          self.$alias_native("data");

          return self.$alias_native("type", "dataType");
        })(self, $scope.get('Event'))
      })(self, null)
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/event/device_light"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass, $gvars = Opal.gvars;

  Opal.add_stubs(['$!', '$nil?', '$[]', '$new', '$to_proc', '$alias_native']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Event(){};
        var self = $Event = $klass($base, $super, 'Event', $Event);

        var def = self.$$proto, $scope = self.$$scope;

        return (function($base, $super) {
          function $DeviceLight(){};
          var self = $DeviceLight = $klass($base, $super, 'DeviceLight', $DeviceLight);

          var def = self.$$proto, $scope = self.$$scope, TMP_1;

          Opal.defs(self, '$supported?', function() {
            var self = this;
            if ($gvars.$ == null) $gvars.$ = nil;

            return $gvars.$['$[]']("DeviceLightEvent")['$nil?']()['$!']();
          });

          (function($base, $super) {
            function $Definition(){};
            var self = $Definition = $klass($base, $super, 'Definition', $Definition);

            var def = self.$$proto, $scope = self.$$scope;

            def["native"] = nil;
            return (def['$value='] = function(value) {
              var self = this;

              return self["native"].value = value;
            }, nil) && 'value='
          })(self, $scope.get('Definition'));

          Opal.defs(self, '$create', TMP_1 = function(name) {
            var $a, $b, self = this, $iter = TMP_1.$$p, block = $iter || nil;

            TMP_1.$$p = null;
            return self.$new(new DeviceLightEvent(name, ($a = ($b = $scope.get('Definition')).$new, $a.$$p = block.$to_proc(), $a).call($b)));
          });

          return self.$alias_native("value");
        })(self, $scope.get('Event'))
      })(self, null)
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/event/device_motion"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass, $gvars = Opal.gvars;

  Opal.add_stubs(['$!', '$nil?', '$[]', '$new', '$to_n', '$to_proc', '$alias_native']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Event(){};
        var self = $Event = $klass($base, $super, 'Event', $Event);

        var def = self.$$proto, $scope = self.$$scope;

        return (function($base, $super) {
          function $DeviceMotion(){};
          var self = $DeviceMotion = $klass($base, $super, 'DeviceMotion', $DeviceMotion);

          var def = self.$$proto, $scope = self.$$scope, TMP_1;

          Opal.defs(self, '$supported?', function() {
            var self = this;
            if ($gvars.$ == null) $gvars.$ = nil;

            return $gvars.$['$[]']("DeviceMotionEvent")['$nil?']()['$!']();
          });

          Opal.cdecl($scope, 'Acceleration', $scope.get('Struct').$new("x", "y", "z"));

          (function($base, $super) {
            function $Definition(){};
            var self = $Definition = $klass($base, $super, 'Definition', $Definition);

            var def = self.$$proto, $scope = self.$$scope;

            def["native"] = nil;
            def['$acceleration='] = function(value) {
              var self = this;

              return self["native"].acceleration = value.$to_n();
            };

            def['$acceleration_with_gravity='] = function(value) {
              var self = this;

              return self["native"].accelerationIncludingGravity = value.$to_n();
            };

            def['$rotation='] = function(value) {
              var self = this;

              return self["native"].rotationRate = value;
            };

            return (def['$interval='] = function(value) {
              var self = this;

              return self["native"].interval = value;
            }, nil) && 'interval=';
          })(self, $scope.get('Definition'));

          Opal.defs(self, '$create', TMP_1 = function(name) {
            var $a, $b, self = this, $iter = TMP_1.$$p, block = $iter || nil;

            TMP_1.$$p = null;
            return self.$new(new DeviceMotionEvent(name, ($a = ($b = $scope.get('Definition')).$new, $a.$$p = block.$to_proc(), $a).call($b)));
          });

          self.$alias_native("acceleration");

          self.$alias_native("acceleration_with_gravity", "accelerationIncludingGravity");

          self.$alias_native("rotation", "rotationRate");

          return self.$alias_native("interval");
        })(self, $scope.get('Event'))
      })(self, null)
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/event/device_orientation"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass, $gvars = Opal.gvars;

  Opal.add_stubs(['$!', '$nil?', '$[]', '$new', '$to_proc', '$alias_native']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Event(){};
        var self = $Event = $klass($base, $super, 'Event', $Event);

        var def = self.$$proto, $scope = self.$$scope;

        return (function($base, $super) {
          function $DeviceOrientation(){};
          var self = $DeviceOrientation = $klass($base, $super, 'DeviceOrientation', $DeviceOrientation);

          var def = self.$$proto, $scope = self.$$scope, TMP_1;

          Opal.defs(self, '$supported?', function() {
            var self = this;
            if ($gvars.$ == null) $gvars.$ = nil;

            return $gvars.$['$[]']("DeviceOrientationEvent")['$nil?']()['$!']();
          });

          (function($base, $super) {
            function $Definition(){};
            var self = $Definition = $klass($base, $super, 'Definition', $Definition);

            var def = self.$$proto, $scope = self.$$scope;

            def["native"] = nil;
            def['$absolute='] = function(value) {
              var self = this;

              return self["native"].absolute = value;
            };

            def['$alpha='] = function(value) {
              var self = this;

              return self["native"].alpha = value;
            };

            def['$beta='] = function(value) {
              var self = this;

              return self["native"].beta = value;
            };

            return (def['$gamma='] = function(value) {
              var self = this;

              return self["native"].gamma = value;
            }, nil) && 'gamma=';
          })(self, $scope.get('Definition'));

          Opal.defs(self, '$create', TMP_1 = function(name) {
            var $a, $b, self = this, $iter = TMP_1.$$p, block = $iter || nil;

            TMP_1.$$p = null;
            return self.$new(new DeviceOrientationEvent(name, ($a = ($b = $scope.get('Definition')).$new, $a.$$p = block.$to_proc(), $a).call($b)));
          });

          self.$alias_native("absolute");

          self.$alias_native("alpha");

          self.$alias_native("beta");

          return self.$alias_native("gamma");
        })(self, $scope.get('Event'))
      })(self, null)
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/event/device_proximity"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass, $gvars = Opal.gvars;

  Opal.add_stubs(['$!', '$nil?', '$[]', '$new', '$to_proc', '$alias_native']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Event(){};
        var self = $Event = $klass($base, $super, 'Event', $Event);

        var def = self.$$proto, $scope = self.$$scope;

        return (function($base, $super) {
          function $DeviceProximity(){};
          var self = $DeviceProximity = $klass($base, $super, 'DeviceProximity', $DeviceProximity);

          var def = self.$$proto, $scope = self.$$scope, TMP_1;

          Opal.defs(self, '$supported?', function() {
            var self = this;
            if ($gvars.$ == null) $gvars.$ = nil;

            return $gvars.$['$[]']("DeviceProximityEvent")['$nil?']()['$!']();
          });

          (function($base, $super) {
            function $Definition(){};
            var self = $Definition = $klass($base, $super, 'Definition', $Definition);

            var def = self.$$proto, $scope = self.$$scope;

            def["native"] = nil;
            def['$value='] = function(value) {
              var self = this;

              return self["native"].value = value;
            };

            def['$min='] = function(value) {
              var self = this;

              return self["native"].min = value;
            };

            return (def['$max='] = function(value) {
              var self = this;

              return self["native"].max = value;
            }, nil) && 'max=';
          })(self, $scope.get('Definition'));

          Opal.defs(self, '$create', TMP_1 = function(name) {
            var $a, $b, self = this, $iter = TMP_1.$$p, block = $iter || nil;

            TMP_1.$$p = null;
            return self.$new(new DeviceProximityEvent(name, ($a = ($b = $scope.get('Definition')).$new, $a.$$p = block.$to_proc(), $a).call($b)));
          });

          self.$alias_native("value");

          self.$alias_native("min");

          return self.$alias_native("max");
        })(self, $scope.get('Event'))
      })(self, null)
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/event/drag"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass, $gvars = Opal.gvars;

  Opal.add_stubs(['$!', '$nil?', '$[]', '$include', '$new', '$try_convert', '$to_proc', '$x', '$screen', '$y', '$DOM']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Event(){};
        var self = $Event = $klass($base, $super, 'Event', $Event);

        var def = self.$$proto, $scope = self.$$scope;

        return (function($base, $super) {
          function $Drag(){};
          var self = $Drag = $klass($base, $super, 'Drag', $Drag);

          var def = self.$$proto, $scope = self.$$scope, TMP_1;

          def["native"] = nil;
          Opal.defs(self, '$supported?', function() {
            var self = this;
            if ($gvars.$ == null) $gvars.$ = nil;

            return $gvars.$['$[]']("DragEvent")['$nil?']()['$!']();
          });

          (function($base, $super) {
            function $Definition(){};
            var self = $Definition = $klass($base, $super, 'Definition', $Definition);

            var def = self.$$proto, $scope = self.$$scope;

            def["native"] = nil;
            (function($base, $super) {
              function $Client(){};
              var self = $Client = $klass($base, $super, 'Client', $Client);

              var def = self.$$proto, $scope = self.$$scope;

              def["native"] = nil;
              self.$include($scope.get('Native'));

              def['$x='] = function(value) {
                var self = this;

                return self["native"].clientX = value;
              };

              return (def['$y='] = function(value) {
                var self = this;

                return self["native"].clientY = value;
              }, nil) && 'y=';
            })(self, null);

            (function($base, $super) {
              function $Screen(){};
              var self = $Screen = $klass($base, $super, 'Screen', $Screen);

              var def = self.$$proto, $scope = self.$$scope;

              def["native"] = nil;
              self.$include($scope.get('Native'));

              def['$x='] = function(value) {
                var self = this;

                return self["native"].screenX = value;
              };

              return (def['$y='] = function(value) {
                var self = this;

                return self["native"].screenY = value;
              }, nil) && 'y=';
            })(self, null);

            def['$alt!'] = function() {
              var self = this;

              return self["native"].altKey = true;
            };

            def['$ctrl!'] = function() {
              var self = this;

              return self["native"].ctrlKey = true;
            };

            def['$meta!'] = function() {
              var self = this;

              return self["native"].metaKey = true;
            };

            def['$button='] = function(value) {
              var self = this;

              return self["native"].button = value;
            };

            def.$client = function() {
              var self = this;

              return $scope.get('Client').$new(self["native"]);
            };

            def.$screen = function() {
              var self = this;

              return $scope.get('Screen').$new(self["native"]);
            };

            return (def['$related='] = function(elem) {
              var self = this;

              return self["native"].relatedTarget = $scope.get('Native').$try_convert(elem);
            }, nil) && 'related=';
          })(self, $scope.get('Definition'));

          Opal.defs(self, '$create', TMP_1 = function(name) {
            var $a, $b, self = this, $iter = TMP_1.$$p, block = $iter || nil;

            TMP_1.$$p = null;
            return self.$new(new DragEvent(name, ($a = ($b = $scope.get('Definition')).$new, $a.$$p = block.$to_proc(), $a).call($b)));
          });

          Opal.cdecl($scope, 'Position', $scope.get('Struct').$new("x", "y"));

          def['$alt?'] = function() {
            var self = this;

            return self["native"].altKey;
          };

          def['$ctrl?'] = function() {
            var self = this;

            return self["native"].ctrlKey;
          };

          def['$meta?'] = function() {
            var self = this;

            return self["native"].metaKey;
          };

          def['$shift?'] = function() {
            var self = this;

            return self["native"].shiftKey;
          };

          def.$button = function() {
            var self = this;

            return self["native"].button;
          };

          def.$client = function() {
            var self = this;

            return $scope.get('Position').$new(self["native"].clientX, self["native"].clientY);
          };

          def.$screen = function() {
            var $a, self = this;

            if ((($a = (typeof(self["native"].screenX) !== "undefined")) !== nil && (!$a.$$is_boolean || $a == true))) {
              return $scope.get('Position').$new(self["native"].screenX, self["native"].screenY)
              } else {
              return nil
            };
          };

          def.$x = function() {
            var self = this;

            return self.$screen().$x();
          };

          def.$y = function() {
            var self = this;

            return self.$screen().$y();
          };

          return (def.$related = function() {
            var self = this;

            return self.$DOM(self["native"].relatedTarget);
          }, nil) && 'related';
        })(self, $scope.get('Event'))
      })(self, null)
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/event/gamepad"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass, $gvars = Opal.gvars;

  Opal.add_stubs(['$!', '$nil?', '$[]', '$new', '$to_proc', '$each', '$define_method']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Event(){};
        var self = $Event = $klass($base, $super, 'Event', $Event);

        var def = self.$$proto, $scope = self.$$scope;

        return (function($base, $super) {
          function $Gamepad(){};
          var self = $Gamepad = $klass($base, $super, 'Gamepad', $Gamepad);

          var def = self.$$proto, $scope = self.$$scope, TMP_2, $a, $b, TMP_3;

          Opal.defs(self, '$supported?', function() {
            var self = this;
            if ($gvars.$ == null) $gvars.$ = nil;

            return $gvars.$['$[]']("GamepadEvent")['$nil?']()['$!']();
          });

          (function($base, $super) {
            function $Definition(){};
            var self = $Definition = $klass($base, $super, 'Definition', $Definition);

            var def = self.$$proto, $scope = self.$$scope, TMP_1;

            def["native"] = nil;
            def.$initialize = TMP_1 = function() {var $zuper = $slice.call(arguments, 0);
              var self = this, $iter = TMP_1.$$p, $yield = $iter || nil;

              TMP_1.$$p = null;
              Opal.find_super_dispatcher(self, 'initialize', TMP_1, $iter).apply(self, $zuper);
              return self["native"].gamepad = {};
            };

            def['$id='] = function(value) {
              var self = this;

              return self["native"].gamepad.id = value;
            };

            def['$index='] = function(value) {
              var self = this;

              return self["native"].gamepad.index = value;
            };

            def['$timestamp='] = function(value) {
              var self = this;

              return self["native"].gamepad.timestamp = value;
            };

            def['$axes='] = function(value) {
              var self = this;

              return self["native"].gamepad.axes = value;
            };

            return (def['$buttons='] = function(value) {
              var self = this;

              return self["native"].gamepad.buttons = value;
            }, nil) && 'buttons=';
          })(self, $scope.get('Definition'));

          Opal.defs(self, '$create', TMP_2 = function(name) {
            var $a, $b, self = this, $iter = TMP_2.$$p, block = $iter || nil;

            TMP_2.$$p = null;
            return self.$new(new GamepadEvent(name, ($a = ($b = $scope.get('Definition')).$new, $a.$$p = block.$to_proc(), $a).call($b)));
          });

          return ($a = ($b = ["id", "index", "timestamp", "axes", "buttons"]).$each, $a.$$p = (TMP_3 = function(name){var self = TMP_3.$$s || this, $a, $b, TMP_4;
if (name == null) name = nil;
          return ($a = ($b = self).$define_method, $a.$$p = (TMP_4 = function(){var self = TMP_4.$$s || this;
              if (self["native"] == null) self["native"] = nil;

            return self["native"].gamepad[name];}, TMP_4.$$s = self, TMP_4), $a).call($b, name)}, TMP_3.$$s = self, TMP_3), $a).call($b);
        })(self, $scope.get('Event'))
      })(self, null)
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/event/hash_change"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass, $gvars = Opal.gvars;

  Opal.add_stubs(['$!', '$nil?', '$[]', '$new', '$to_proc', '$alias_native']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Event(){};
        var self = $Event = $klass($base, $super, 'Event', $Event);

        var def = self.$$proto, $scope = self.$$scope;

        return (function($base, $super) {
          function $HashChange(){};
          var self = $HashChange = $klass($base, $super, 'HashChange', $HashChange);

          var def = self.$$proto, $scope = self.$$scope, TMP_1;

          Opal.defs(self, '$supported?', function() {
            var self = this;
            if ($gvars.$ == null) $gvars.$ = nil;

            return $gvars.$['$[]']("HashChangeEvent")['$nil?']()['$!']();
          });

          (function($base, $super) {
            function $Definition(){};
            var self = $Definition = $klass($base, $super, 'Definition', $Definition);

            var def = self.$$proto, $scope = self.$$scope;

            def["native"] = nil;
            def['$old='] = function(value) {
              var self = this;

              return self["native"].oldURL = value;
            };

            return (def['$new='] = function(value) {
              var self = this;

              return self["native"].newURL = value;
            }, nil) && 'new=';
          })(self, $scope.get('Definition'));

          Opal.defs(self, '$create', TMP_1 = function(name) {
            var $a, $b, self = this, $iter = TMP_1.$$p, block = $iter || nil;

            TMP_1.$$p = null;
            return self.$new(new HashChangeEvent(name, ($a = ($b = $scope.get('Definition')).$new, $a.$$p = block.$to_proc(), $a).call($b)));
          });

          self.$alias_native("old", "oldURL");

          return self.$alias_native("new", "newURL");
        })(self, $scope.get('Event'))
      })(self, null)
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/event/progress"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass, $gvars = Opal.gvars;

  Opal.add_stubs(['$!', '$nil?', '$[]', '$new', '$to_proc', '$alias_native']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Event(){};
        var self = $Event = $klass($base, $super, 'Event', $Event);

        var def = self.$$proto, $scope = self.$$scope;

        return (function($base, $super) {
          function $Progress(){};
          var self = $Progress = $klass($base, $super, 'Progress', $Progress);

          var def = self.$$proto, $scope = self.$$scope, TMP_1;

          Opal.defs(self, '$supported?', function() {
            var self = this;
            if ($gvars.$ == null) $gvars.$ = nil;

            return $gvars.$['$[]']("ProgressEvent")['$nil?']()['$!']();
          });

          (function($base, $super) {
            function $Definition(){};
            var self = $Definition = $klass($base, $super, 'Definition', $Definition);

            var def = self.$$proto, $scope = self.$$scope;

            def["native"] = nil;
            def['$computable='] = function(value) {
              var self = this;

              return self["native"].computableLength = value;
            };

            def['$loaded='] = function(value) {
              var self = this;

              return self["native"].loaded = value;
            };

            return (def['$total='] = function(value) {
              var self = this;

              return self["native"].total = value;
            }, nil) && 'total=';
          })(self, $scope.get('Definition'));

          Opal.defs(self, '$create', TMP_1 = function(name) {
            var $a, $b, self = this, $iter = TMP_1.$$p, block = $iter || nil;

            TMP_1.$$p = null;
            return self.$new(new ProgressEvent(name, ($a = ($b = $scope.get('Definition')).$new, $a.$$p = block.$to_proc(), $a).call($b)));
          });

          self.$alias_native("computable?", "computableLength");

          self.$alias_native("loaded");

          return self.$alias_native("total");
        })(self, $scope.get('Event'))
      })(self, null)
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/event/page_transition"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass, $gvars = Opal.gvars;

  Opal.add_stubs(['$!', '$nil?', '$[]', '$new', '$to_proc', '$alias_native']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Event(){};
        var self = $Event = $klass($base, $super, 'Event', $Event);

        var def = self.$$proto, $scope = self.$$scope;

        return (function($base, $super) {
          function $PageTransition(){};
          var self = $PageTransition = $klass($base, $super, 'PageTransition', $PageTransition);

          var def = self.$$proto, $scope = self.$$scope, TMP_1;

          Opal.defs(self, '$supported?', function() {
            var self = this;
            if ($gvars.$ == null) $gvars.$ = nil;

            return $gvars.$['$[]']("PageTransitionEvent")['$nil?']()['$!']();
          });

          (function($base, $super) {
            function $Definition(){};
            var self = $Definition = $klass($base, $super, 'Definition', $Definition);

            var def = self.$$proto, $scope = self.$$scope;

            def["native"] = nil;
            return (def['$persisted='] = function(value) {
              var self = this;

              return self["native"].persisted = value;
            }, nil) && 'persisted='
          })(self, $scope.get('Definition'));

          Opal.defs(self, '$create', TMP_1 = function(name) {
            var $a, $b, self = this, $iter = TMP_1.$$p, block = $iter || nil;

            TMP_1.$$p = null;
            return self.$new(new PageTransitionEvent(name, ($a = ($b = $scope.get('Definition')).$new, $a.$$p = block.$to_proc(), $a).call($b)));
          });

          return self.$alias_native("persisted?", "persisted");
        })(self, $scope.get('Event'))
      })(self, null)
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/event/pop_state"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass, $gvars = Opal.gvars;

  Opal.add_stubs(['$!', '$nil?', '$[]', '$new', '$to_proc', '$alias_native']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Event(){};
        var self = $Event = $klass($base, $super, 'Event', $Event);

        var def = self.$$proto, $scope = self.$$scope;

        return (function($base, $super) {
          function $PopState(){};
          var self = $PopState = $klass($base, $super, 'PopState', $PopState);

          var def = self.$$proto, $scope = self.$$scope, TMP_1;

          Opal.defs(self, '$supported?', function() {
            var self = this;
            if ($gvars.$ == null) $gvars.$ = nil;

            return $gvars.$['$[]']("PopStateEvent")['$nil?']()['$!']();
          });

          (function($base, $super) {
            function $Definition(){};
            var self = $Definition = $klass($base, $super, 'Definition', $Definition);

            var def = self.$$proto, $scope = self.$$scope;

            def["native"] = nil;
            return (def['$state='] = function(value) {
              var self = this;

              return self["native"].state = value;
            }, nil) && 'state='
          })(self, $scope.get('Definition'));

          Opal.defs(self, '$create', TMP_1 = function(name) {
            var $a, $b, self = this, $iter = TMP_1.$$p, block = $iter || nil;

            TMP_1.$$p = null;
            return self.$new(new PopStateEvent(name, ($a = ($b = $scope.get('Definition')).$new, $a.$$p = block.$to_proc(), $a).call($b)));
          });

          return self.$alias_native("state");
        })(self, $scope.get('Event'))
      })(self, null)
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/event/storage"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass, $gvars = Opal.gvars;

  Opal.add_stubs(['$!', '$nil?', '$[]', '$new', '$to_proc', '$alias_native']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Event(){};
        var self = $Event = $klass($base, $super, 'Event', $Event);

        var def = self.$$proto, $scope = self.$$scope;

        return (function($base, $super) {
          function $Storage(){};
          var self = $Storage = $klass($base, $super, 'Storage', $Storage);

          var def = self.$$proto, $scope = self.$$scope, TMP_1;

          Opal.defs(self, '$supported?', function() {
            var self = this;
            if ($gvars.$ == null) $gvars.$ = nil;

            return $gvars.$['$[]']("StorageEvent")['$nil?']()['$!']();
          });

          (function($base, $super) {
            function $Definition(){};
            var self = $Definition = $klass($base, $super, 'Definition', $Definition);

            var def = self.$$proto, $scope = self.$$scope;

            def["native"] = nil;
            def['$key='] = function(value) {
              var self = this;

              return self["native"].key = value;
            };

            def['$new='] = function(value) {
              var self = this;

              return self["native"].newValue = value;
            };

            def['$old='] = function(value) {
              var self = this;

              return self["native"].oldValue = value;
            };

            def['$area='] = function(value) {
              var self = this;

              return self["native"].storageArea = value;
            };

            return (def['$url='] = function(value) {
              var self = this;

              return self["native"].url = value;
            }, nil) && 'url=';
          })(self, $scope.get('Definition'));

          Opal.defs(self, '$create', TMP_1 = function(name) {
            var $a, $b, self = this, $iter = TMP_1.$$p, block = $iter || nil;

            TMP_1.$$p = null;
            return self.$new(new StorageEvent(name, ($a = ($b = $scope.get('Definition')).$new, $a.$$p = block.$to_proc(), $a).call($b)));
          });

          self.$alias_native("key");

          self.$alias_native("new", "newValue");

          self.$alias_native("old", "oldValue");

          self.$alias_native("area", "storageArea");

          return self.$alias_native("url");
        })(self, $scope.get('Event'))
      })(self, null)
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/event/touch"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass, $gvars = Opal.gvars;

  Opal.add_stubs(['$!', '$nil?', '$[]', '$new', '$to_proc', '$==', '$downcase', '$name']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Event(){};
        var self = $Event = $klass($base, $super, 'Event', $Event);

        var def = self.$$proto, $scope = self.$$scope;

        return (function($base, $super) {
          function $Touch(){};
          var self = $Touch = $klass($base, $super, 'Touch', $Touch);

          var def = self.$$proto, $scope = self.$$scope, TMP_1;

          def["native"] = nil;
          Opal.defs(self, '$supported?', function() {
            var self = this;
            if ($gvars.$ == null) $gvars.$ = nil;

            return $gvars.$['$[]']("TouchEvent")['$nil?']()['$!']();
          });

          (function($base, $super) {
            function $Definition(){};
            var self = $Definition = $klass($base, $super, 'Definition', $Definition);

            var def = self.$$proto, $scope = self.$$scope;

            def["native"] = nil;
            def['$alt!'] = function() {
              var self = this;

              return self["native"].altKey = true;
            };

            def['$ctrl!'] = function() {
              var self = this;

              return self["native"].ctrlKey = true;
            };

            def['$meta!'] = function() {
              var self = this;

              return self["native"].metaKey = true;
            };

            return (def['$shift!'] = function() {
              var self = this;

              return self["native"].shiftKey = true;
            }, nil) && 'shift!';
          })(self, $scope.get('Definition'));

          Opal.defs(self, '$create', TMP_1 = function(name) {
            var $a, $b, self = this, $iter = TMP_1.$$p, block = $iter || nil;

            TMP_1.$$p = null;
            return self.$new(new TouchEvent(name, ($a = ($b = $scope.get('Definition')).$new, $a.$$p = block.$to_proc(), $a).call($b)));
          });

          def['$alt?'] = function() {
            var self = this;

            return self["native"].altKey;
          };

          def['$ctrl?'] = function() {
            var self = this;

            return self["native"].ctrlKey;
          };

          def['$meta?'] = function() {
            var self = this;

            return self["native"].metaKey;
          };

          def['$shift?'] = function() {
            var self = this;

            return self["native"].shiftKey;
          };

          def['$cancel?'] = function() {
            var self = this;

            return self.$name().$downcase()['$==']("touchcancel");
          };

          def['$end?'] = function() {
            var self = this;

            return self.$name().$downcase()['$==']("touchend");
          };

          def['$leave?'] = function() {
            var self = this;

            return self.$name().$downcase()['$==']("touchleave");
          };

          def['$move?'] = function() {
            var self = this;

            return self.$name().$downcase()['$==']("touchmove");
          };

          return (def['$start?'] = function() {
            var self = this;

            return self.$name().$downcase()['$==']("touchstart");
          }, nil) && 'start?';
        })(self, $scope.get('Event'))
      })(self, null)
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/event/sensor"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass, $gvars = Opal.gvars;

  Opal.add_stubs(['$!', '$nil?', '$[]', '$new', '$to_proc']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Event(){};
        var self = $Event = $klass($base, $super, 'Event', $Event);

        var def = self.$$proto, $scope = self.$$scope;

        return (function($base, $super) {
          function $Sensor(){};
          var self = $Sensor = $klass($base, $super, 'Sensor', $Sensor);

          var def = self.$$proto, $scope = self.$$scope, TMP_1;

          Opal.defs(self, '$supported?', function() {
            var self = this;
            if ($gvars.$ == null) $gvars.$ = nil;

            return $gvars.$['$[]']("SensorEvent")['$nil?']()['$!']();
          });

          return (Opal.defs(self, '$create', TMP_1 = function(name) {
            var $a, $b, self = this, $iter = TMP_1.$$p, block = $iter || nil;

            TMP_1.$$p = null;
            return self.$new(new SensorEvent(name, ($a = ($b = $scope.get('Definition')).$new, $a.$$p = block.$to_proc(), $a).call($b)));
          }), nil) && 'create';
        })(self, $scope.get('Event'))
      })(self, null)
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["ostruct"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass, $hash2 = Opal.hash2, $range = Opal.range;

  Opal.add_stubs(['$each_pair', '$[]=', '$to_sym', '$[]', '$end_with?', '$enum_for', '$is_a?', '$==', '$instance_variable_get', '$===', '$eql?', '$dup', '$to_n', '$hash', '$class', '$join', '$map', '$inspect']);
  return (function($base, $super) {
    function $OpenStruct(){};
    var self = $OpenStruct = $klass($base, $super, 'OpenStruct', $OpenStruct);

    var def = self.$$proto, $scope = self.$$scope, TMP_2;

    def.table = nil;
    def.$initialize = function(hash) {
      var $a, $b, TMP_1, self = this;

      if (hash == null) {
        hash = nil
      }
      self.table = $hash2([], {});
      if (hash !== false && hash !== nil) {
        return ($a = ($b = hash).$each_pair, $a.$$p = (TMP_1 = function(key, value){var self = TMP_1.$$s || this;
          if (self.table == null) self.table = nil;
if (key == null) key = nil;if (value == null) value = nil;
        return self.table['$[]='](key.$to_sym(), value)}, TMP_1.$$s = self, TMP_1), $a).call($b)
        } else {
        return nil
      };
    };

    def['$[]'] = function(name) {
      var self = this;

      return self.table['$[]'](name.$to_sym());
    };

    def['$[]='] = function(name, value) {
      var self = this;

      return self.table['$[]='](name.$to_sym(), value);
    };

    def.$method_missing = function(name, args) {
      var $a, self = this;

      args = $slice.call(arguments, 1);
      if ((($a = name['$end_with?']("=")) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self.table['$[]='](name['$[]']($range(0, -2, false)).$to_sym(), args['$[]'](0))
        } else {
        return self.table['$[]'](name.$to_sym())
      };
    };

    def.$each_pair = TMP_2 = function() {
      var $a, $b, TMP_3, self = this, $iter = TMP_2.$$p, $yield = $iter || nil;

      TMP_2.$$p = null;
      if (($yield !== nil)) {
        } else {
        return self.$enum_for("each_pair")
      };
      return ($a = ($b = self.table).$each_pair, $a.$$p = (TMP_3 = function(pair){var self = TMP_3.$$s || this, $a;
if (pair == null) pair = nil;
      return $a = Opal.yield1($yield, pair), $a === $breaker ? $a : $a}, TMP_3.$$s = self, TMP_3), $a).call($b);
    };

    def['$=='] = function(other) {
      var $a, self = this;

      if ((($a = other['$is_a?']($scope.get('OpenStruct'))) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        return false
      };
      return self.table['$=='](other.$instance_variable_get("@table"));
    };

    def['$==='] = function(other) {
      var $a, self = this;

      if ((($a = other['$is_a?']($scope.get('OpenStruct'))) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        return false
      };
      return self.table['$==='](other.$instance_variable_get("@table"));
    };

    def['$eql?'] = function(other) {
      var $a, self = this;

      if ((($a = other['$is_a?']($scope.get('OpenStruct'))) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        return false
      };
      return self.table['$eql?'](other.$instance_variable_get("@table"));
    };

    def.$to_h = function() {
      var self = this;

      return self.table.$dup();
    };

    def.$to_n = function() {
      var self = this;

      return self.table.$to_n();
    };

    def.$hash = function() {
      var self = this;

      return self.table.$hash();
    };

    return (def.$inspect = function() {
      var $a, $b, TMP_4, self = this;

      return "#<" + (self.$class()) + ": " + (($a = ($b = self.$each_pair()).$map, $a.$$p = (TMP_4 = function(name, value){var self = TMP_4.$$s || this;
if (name == null) name = nil;if (value == null) value = nil;
      return "" + (name) + "=" + (self['$[]'](name).$inspect())}, TMP_4.$$s = self, TMP_4), $a).call($b).$join(" ")) + ">";
    }, nil) && 'inspect';
  })(self, null)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/event/custom"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass, $gvars = Opal.gvars;

  Opal.add_stubs(['$require', '$!', '$nil?', '$[]', '$new', '$call', '$to_n', '$has_key?']);
  self.$require("ostruct");
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Event(){};
        var self = $Event = $klass($base, $super, 'Event', $Event);

        var def = self.$$proto, $scope = self.$$scope;

        return (function($base, $super) {
          function $Custom(){};
          var self = $Custom = $klass($base, $super, 'Custom', $Custom);

          var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_2, TMP_3;

          def.detail = nil;
          Opal.defs(self, '$supported?', function() {
            var self = this;
            if ($gvars.$ == null) $gvars.$ = nil;

            return $gvars.$['$[]']("CustomEvent")['$nil?']()['$!']();
          });

          Opal.defs(self, '$create', TMP_1 = function(name) {
            var self = this, $iter = TMP_1.$$p, block = $iter || nil, data = nil;

            TMP_1.$$p = null;
            data = $scope.get('OpenStruct').$new();
            if (block !== false && block !== nil) {
              block.$call(data)};
            return self.$new(new CustomEvent(name, { detail: data.$to_n() }));
          });

          def.$initialize = TMP_2 = function(native$) {
            var self = this, $iter = TMP_2.$$p, $yield = $iter || nil;

            TMP_2.$$p = null;
            Opal.find_super_dispatcher(self, 'initialize', TMP_2, null).apply(self, [native$]);
            self["native"] = native$;
            return self.detail = $scope.get('Hash').$new(native$.detail);
          };

          return (def.$method_missing = TMP_3 = function(id) {var $zuper = $slice.call(arguments, 0);
            var $a, self = this, $iter = TMP_3.$$p, $yield = $iter || nil;

            TMP_3.$$p = null;
            if ((($a = self.detail['$has_key?'](id)) !== nil && (!$a.$$is_boolean || $a == true))) {
              return self.detail['$[]'](id)};
            return Opal.find_super_dispatcher(self, 'method_missing', TMP_3, $iter).apply(self, $zuper);
          }, nil) && 'method_missing';
        })(self, $scope.get('Event'))
      })(self, null)
    })(self)
  })(self);
};

/* Generated by Opal 0.7.2 */
Opal.modules["buffer/array"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass, $gvars = Opal.gvars;

  Opal.add_stubs(['$[]', '$name_for', '$include', '$attr_reader', '$==', '$for', '$to_n', '$enum_for']);
  return (function($base, $super) {
    function $Buffer(){};
    var self = $Buffer = $klass($base, $super, 'Buffer', $Buffer);

    var def = self.$$proto, $scope = self.$$scope;

    return (function($base, $super) {
      function $Array(){};
      var self = $Array = $klass($base, $super, 'Array', $Array);

      var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_2;

      def["native"] = nil;
      Opal.defs(self, '$for', function(bits, type) {
        var self = this;
        if ($gvars.$ == null) $gvars.$ = nil;

        return $gvars.$['$[]']("" + ($scope.get('Buffer').$name_for(bits, type)) + "Array");
      });

      self.$include($scope.get('Enumerable'));

      self.$attr_reader("buffer", "type");

      def.$initialize = TMP_1 = function(buffer, bits, type) {
        var self = this, $iter = TMP_1.$$p, $yield = $iter || nil;

        if (bits == null) {
          bits = nil
        }
        if (type == null) {
          type = nil
        }
        TMP_1.$$p = null;
        if ($scope.get('Native')['$=='](buffer)) {
          Opal.find_super_dispatcher(self, 'initialize', TMP_1, null).apply(self, [buffer])
          } else {
          
        var klass = $scope.get('Array').$for(bits, type);

        Opal.find_super_dispatcher(self, 'initialize', TMP_1, null).apply(self, [new klass(buffer.$to_n())])
      ;
        };
        self.buffer = buffer;
        return self.type = type;
      };

      def.$bits = function() {
        var self = this;

        return self["native"].BYTES_PER_ELEMENT * 8;
      };

      def['$[]'] = function(index, offset) {
        var self = this;

        if (offset == null) {
          offset = nil
        }
        if (offset !== false && offset !== nil) {
          return self["native"].subarray(index, offset);
          } else {
          return self["native"][index];
        };
      };

      def['$[]='] = function(index, value) {
        var self = this;

        return self["native"][index] = value;
      };

      def.$bytesize = function() {
        var self = this;

        return self["native"].byteLength;
      };

      def.$each = TMP_2 = function() {
        var $a, self = this, $iter = TMP_2.$$p, $yield = $iter || nil;

        TMP_2.$$p = null;
        if (($yield !== nil)) {
          } else {
          return self.$enum_for("each")
        };
        
      for (var i = 0, length = self["native"].length; i < length; i++) {
        ((($a = Opal.yield1($yield, self["native"][i])) === $breaker) ? $breaker.$v : $a)
      }
    ;
        return self;
      };

      def.$length = function() {
        var self = this;

        return self["native"].length;
      };

      def['$merge!'] = function(other, offset) {
        var self = this;

        return self["native"].set(other.$to_n(), offset);
      };

      return Opal.defn(self, '$size', def.$length);
    })(self, $scope.get('Native'))
  })(self, $scope.get('Native'))
};

/* Generated by Opal 0.7.2 */
Opal.modules["buffer/view"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass, $gvars = Opal.gvars;

  Opal.add_stubs(['$include', '$!', '$nil?', '$[]', '$attr_reader', '$native?', '$to_n', '$name_for']);
  return (function($base, $super) {
    function $Buffer(){};
    var self = $Buffer = $klass($base, $super, 'Buffer', $Buffer);

    var def = self.$$proto, $scope = self.$$scope;

    return (function($base, $super) {
      function $View(){};
      var self = $View = $klass($base, $super, 'View', $View);

      var def = self.$$proto, $scope = self.$$scope, TMP_1;

      def["native"] = nil;
      self.$include($scope.get('Native'));

      Opal.defs(self, '$supported?', function() {
        var self = this;
        if ($gvars.$ == null) $gvars.$ = nil;

        return $gvars.$['$[]']("DataView")['$nil?']()['$!']();
      });

      self.$attr_reader("buffer", "offset");

      def.$initialize = TMP_1 = function(buffer, offset, length) {
        var $a, $b, self = this, $iter = TMP_1.$$p, $yield = $iter || nil;

        if (offset == null) {
          offset = nil
        }
        if (length == null) {
          length = nil
        }
        TMP_1.$$p = null;
        if ((($a = self['$native?'](buffer)) !== nil && (!$a.$$is_boolean || $a == true))) {
          Opal.find_super_dispatcher(self, 'initialize', TMP_1, null).apply(self, [buffer])
        } else if ((($a = (($b = offset !== false && offset !== nil) ? length : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
          Opal.find_super_dispatcher(self, 'initialize', TMP_1, null).apply(self, [new DataView(buffer.$to_n(), offset.$to_n(), length.$to_n())])
        } else if (offset !== false && offset !== nil) {
          Opal.find_super_dispatcher(self, 'initialize', TMP_1, null).apply(self, [new DataView(buffer.$to_n(), offset.$to_n())])
          } else {
          Opal.find_super_dispatcher(self, 'initialize', TMP_1, null).apply(self, [new DataView(buffer.$to_n())])
        };
        self.buffer = buffer;
        return self.offset = offset;
      };

      def.$length = function() {
        var self = this;

        return self["native"].byteLength;
      };

      Opal.defn(self, '$size', def.$length);

      def.$get = function(offset, bits, type, little) {
        var self = this;

        if (bits == null) {
          bits = 8
        }
        if (type == null) {
          type = "unsigned"
        }
        if (little == null) {
          little = false
        }
        return self["native"]["get" + $scope.get('Buffer').$name_for(bits, type)](offset, little);
      };

      Opal.defn(self, '$[]', def.$get);

      def.$set = function(offset, value, bits, type, little) {
        var self = this;

        if (bits == null) {
          bits = 8
        }
        if (type == null) {
          type = "unsigned"
        }
        if (little == null) {
          little = false
        }
        return self["native"]["set" + $scope.get('Buffer').$name_for(bits, type)](offset, value, little);
      };

      Opal.defn(self, '$[]=', def.$set);

      def.$get_int8 = function(offset, little) {
        var self = this;

        if (little == null) {
          little = false
        }
        return self["native"].getInt8(offset, little);
      };

      def.$set_int8 = function(offset, value, little) {
        var self = this;

        if (little == null) {
          little = false
        }
        return self["native"].setInt8(offset, value, little);
      };

      def.$get_uint8 = function(offset, little) {
        var self = this;

        if (little == null) {
          little = false
        }
        return self["native"].getUint8(offset, little);
      };

      def.$set_uint8 = function(offset, value, little) {
        var self = this;

        if (little == null) {
          little = false
        }
        return self["native"].setUint8(offset, value, little);
      };

      def.$get_int16 = function(offset, little) {
        var self = this;

        if (little == null) {
          little = false
        }
        return self["native"].getInt16(offset, little);
      };

      def.$set_int16 = function(offset, value, little) {
        var self = this;

        if (little == null) {
          little = false
        }
        return self["native"].setInt16(offset, value, little);
      };

      def.$get_uint16 = function(offset, little) {
        var self = this;

        if (little == null) {
          little = false
        }
        return self["native"].getUint16(offset, little);
      };

      def.$set_uint16 = function(offset, value, little) {
        var self = this;

        if (little == null) {
          little = false
        }
        return self["native"].setUint16(offset, value, little);
      };

      def.$get_int32 = function(offset, little) {
        var self = this;

        if (little == null) {
          little = false
        }
        return self["native"].getInt32(offset, little);
      };

      def.$set_int32 = function(offset, value, little) {
        var self = this;

        if (little == null) {
          little = false
        }
        return self["native"].setInt32(offset, value, little);
      };

      def.$get_uint32 = function(offset, little) {
        var self = this;

        if (little == null) {
          little = false
        }
        return self["native"].getUint32(offset, little);
      };

      def.$set_uint32 = function(offset, value, little) {
        var self = this;

        if (little == null) {
          little = false
        }
        return self["native"].setUint32(offset, value, little);
      };

      def.$get_float32 = function(offset, little) {
        var self = this;

        if (little == null) {
          little = false
        }
        return self["native"].getFloat32(offset, little);
      };

      def.$set_float32 = function(offset, value, little) {
        var self = this;

        if (little == null) {
          little = false
        }
        return self["native"].setFloat32(offset, value, little);
      };

      def.$get_float64 = function(offset, little) {
        var self = this;

        if (little == null) {
          little = false
        }
        return self["native"].getFloat64(offset, little);
      };

      return (def.$set_float64 = function(offset, value, little) {
        var self = this;

        if (little == null) {
          little = false
        }
        return self["native"].setFloat64(offset, value, little);
      }, nil) && 'set_float64';
    })(self, null)
  })(self, null)
};

/* Generated by Opal 0.7.2 */
Opal.modules["buffer"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass, $gvars = Opal.gvars;

  Opal.add_stubs(['$require', '$include', '$!', '$nil?', '$[]', '$===', '$native?', '$new']);
  self.$require("native");
  self.$require("buffer/array");
  self.$require("buffer/view");
  return (function($base, $super) {
    function $Buffer(){};
    var self = $Buffer = $klass($base, $super, 'Buffer', $Buffer);

    var def = self.$$proto, $scope = self.$$scope, TMP_1;

    def["native"] = nil;
    self.$include($scope.get('Native'));

    Opal.defs(self, '$supported?', function() {
      var self = this;
      if ($gvars.$ == null) $gvars.$ = nil;

      return $gvars.$['$[]']("ArrayBuffer")['$nil?']()['$!']();
    });

    Opal.defs(self, '$name_for', function(bits, type) {
      var self = this, $case = nil;

      return "" + ((function() {$case = type;if ("unsigned"['$===']($case)) {return "Uint"}else if ("signed"['$===']($case)) {return "Int"}else if ("float"['$===']($case)) {return "Float"}else { return nil }})()) + (bits);
    });

    def.$initialize = TMP_1 = function(size, bits) {
      var $a, self = this, $iter = TMP_1.$$p, $yield = $iter || nil;

      if (bits == null) {
        bits = 8
      }
      TMP_1.$$p = null;
      if ((($a = self['$native?'](size)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return Opal.find_super_dispatcher(self, 'initialize', TMP_1, null).apply(self, [size])
        } else {
        return Opal.find_super_dispatcher(self, 'initialize', TMP_1, null).apply(self, [new ArrayBuffer(size * (bits / 8))])
      };
    };

    def.$length = function() {
      var self = this;

      return self["native"].byteLength;
    };

    Opal.defn(self, '$size', def.$length);

    def.$to_a = function(bits, type) {
      var self = this;

      if (bits == null) {
        bits = 8
      }
      if (type == null) {
        type = "unsigned"
      }
      return $scope.get('Array').$new(self, bits, type);
    };

    return (def.$view = function(offset, length) {
      var self = this;

      if (offset == null) {
        offset = nil
      }
      if (length == null) {
        length = nil
      }
      return $scope.get('View').$new(self, offset, length);
    }, nil) && 'view';
  })(self, null);
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/event/message"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass, $gvars = Opal.gvars;

  Opal.add_stubs(['$require', '$!', '$nil?', '$[]', '$to_n', '$new', '$to_proc', '$alias_native']);
  self.$require("buffer");
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Event(){};
        var self = $Event = $klass($base, $super, 'Event', $Event);

        var def = self.$$proto, $scope = self.$$scope;

        return (function($base, $super) {
          function $Message(){};
          var self = $Message = $klass($base, $super, 'Message', $Message);

          var def = self.$$proto, $scope = self.$$scope, TMP_1;

          def["native"] = nil;
          Opal.defs(self, '$supported?', function() {
            var self = this;
            if ($gvars.$ == null) $gvars.$ = nil;

            return $gvars.$['$[]']("MessageEvent")['$nil?']()['$!']();
          });

          (function($base, $super) {
            function $Definition(){};
            var self = $Definition = $klass($base, $super, 'Definition', $Definition);

            var def = self.$$proto, $scope = self.$$scope;

            def["native"] = nil;
            return (def['$data='] = function(value) {
              var self = this;

              return self["native"].data = value.$to_n();
            }, nil) && 'data='
          })(self, $scope.get('Definition'));

          Opal.defs(self, '$create', TMP_1 = function(name) {
            var $a, $b, self = this, $iter = TMP_1.$$p, block = $iter || nil;

            TMP_1.$$p = null;
            return self.$new(new MessageEvent(name, ($a = ($b = $scope.get('Definition')).$new, $a.$$p = block.$to_proc(), $a).call($b)));
          });

          def.$data = function() {
            var self = this;

            
      if (self["native"].data instanceof ArrayBuffer) {
        return $scope.get('Buffer').$new(self["native"].data);
      }
      else if (self["native"].data instanceof Blob) {
        return $scope.get('Blob').$new(self["native"].data);
      }
      else {
        return self["native"].data;
      }
    ;
          };

          self.$alias_native("origin");

          return (def.$source = function() {
            var self = this;

            
      var source = self["native"].source;

      if (source instanceof window.Window) {
        return $scope.get('Window').$new(source);
      }
      else {
        return nil;
      }
    ;
          }, nil) && 'source';
        })(self, $scope.get('Event'))
      })(self, null)
    })(self)
  })(self);
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/event/close"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass, $gvars = Opal.gvars;

  Opal.add_stubs(['$!', '$nil?', '$[]', '$new', '$to_proc', '$alias_native']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Event(){};
        var self = $Event = $klass($base, $super, 'Event', $Event);

        var def = self.$$proto, $scope = self.$$scope;

        return (function($base, $super) {
          function $Close(){};
          var self = $Close = $klass($base, $super, 'Close', $Close);

          var def = self.$$proto, $scope = self.$$scope, TMP_1;

          Opal.defs(self, '$supported?', function() {
            var self = this;
            if ($gvars.$ == null) $gvars.$ = nil;

            return $gvars.$['$[]']("CloseEvent")['$nil?']()['$!']();
          });

          (function($base, $super) {
            function $Definition(){};
            var self = $Definition = $klass($base, $super, 'Definition', $Definition);

            var def = self.$$proto, $scope = self.$$scope;

            def["native"] = nil;
            def['$code='] = function(value) {
              var self = this;

              return self["native"].code = value;
            };

            def['$reason='] = function(value) {
              var self = this;

              return self["native"].reason = value;
            };

            def['$clean!'] = function(value) {
              var self = this;

              return self["native"].wasClean = true;
            };

            return (def['$not_clean!'] = function(value) {
              var self = this;

              return self["native"].wasClean = false;
            }, nil) && 'not_clean!';
          })(self, $scope.get('Definition'));

          Opal.defs(self, '$create', TMP_1 = function(name) {
            var $a, $b, self = this, $iter = TMP_1.$$p, block = $iter || nil;

            TMP_1.$$p = null;
            return self.$new(new CloseEvent(name, ($a = ($b = $scope.get('Definition')).$new, $a.$$p = block.$to_proc(), $a).call($b)));
          });

          self.$alias_native("code");

          self.$alias_native("reason");

          return self.$alias_native("clean?", "wasClean");
        })(self, $scope.get('Event'))
      })(self, null)
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/event"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass, $hash2 = Opal.hash2, $hash = Opal.hash, $gvars = Opal.gvars;

  Opal.add_stubs(['$require', '$new', '$merge!', '$gsub', '$[]', '$names', '$name_for', '$===', '$!', '$==', '$supported?', '$class_for', '$to_proc', '$create', '$arguments=', '$find', '$is_a?', '$classes', '$attr_reader', '$convert', '$off', '$alias_native']);
  self.$require("browser/dom/event/base");
  self.$require("browser/dom/event/ui");
  self.$require("browser/dom/event/mouse");
  self.$require("browser/dom/event/keyboard");
  self.$require("browser/dom/event/focus");
  self.$require("browser/dom/event/wheel");
  self.$require("browser/dom/event/composition");
  self.$require("browser/dom/event/animation");
  self.$require("browser/dom/event/audio_processing");
  self.$require("browser/dom/event/before_unload");
  self.$require("browser/dom/event/composition");
  self.$require("browser/dom/event/clipboard");
  self.$require("browser/dom/event/device_light");
  self.$require("browser/dom/event/device_motion");
  self.$require("browser/dom/event/device_orientation");
  self.$require("browser/dom/event/device_proximity");
  self.$require("browser/dom/event/drag");
  self.$require("browser/dom/event/gamepad");
  self.$require("browser/dom/event/hash_change");
  self.$require("browser/dom/event/progress");
  self.$require("browser/dom/event/page_transition");
  self.$require("browser/dom/event/pop_state");
  self.$require("browser/dom/event/storage");
  self.$require("browser/dom/event/touch");
  self.$require("browser/dom/event/sensor");
  self.$require("browser/dom/event/custom");
  self.$require("browser/dom/event/message");
  self.$require("browser/dom/event/close");
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Event(){};
        var self = $Event = $klass($base, $super, 'Event', $Event);

        var def = self.$$proto, $scope = self.$$scope, TMP_2, TMP_4, TMP_5;

        def["native"] = def.callback = nil;
        Opal.defs(self, '$names', function() {
          var $a, $b, TMP_1, self = this;
          if (self.names == null) self.names = nil;

          if ((($a = self.names) !== nil && (!$a.$$is_boolean || $a == true))) {
            return self.names};
          self.names = ($a = ($b = $scope.get('Hash')).$new, $a.$$p = (TMP_1 = function(_, k){var self = TMP_1.$$s || this;
if (_ == null) _ = nil;if (k == null) k = nil;
          return k}, TMP_1.$$s = self, TMP_1), $a).call($b);
          return self.names['$merge!']($hash2(["load", "hover"], {"load": "DOMContentLoaded", "hover": "mouse:over"}));
        });

        Opal.defs(self, '$name_for', function(name) {
          var self = this;

          return self.$names()['$[]'](name).$gsub(":", "");
        });

        Opal.defs(self, '$classes', function() {
          var $a, self = this;
          if (self.classes == null) self.classes = nil;
          if ($gvars.$ == null) $gvars.$ = nil;

          return ((($a = self.classes) !== false && $a !== nil) ? $a : self.classes = $hash($scope.get('Animation'), $gvars.$['$[]']("AnimationEvent"), $scope.get('AudioProcessing'), $gvars.$['$[]']("AudioProcessingEvent"), $scope.get('BeforeUnload'), $gvars.$['$[]']("BeforeUnloadEvent"), $scope.get('Composition'), $gvars.$['$[]']("CompositionEvent"), $scope.get('Clipboard'), $gvars.$['$[]']("ClipboardEvent"), $scope.get('DeviceLight'), $gvars.$['$[]']("DeviceLightEvent"), $scope.get('DeviceMotion'), $gvars.$['$[]']("DeviceMotionEvent"), $scope.get('DeviceOrientation'), $gvars.$['$[]']("DeviceOrientationEvent"), $scope.get('DeviceProximity'), $gvars.$['$[]']("DeviceProximityEvent"), $scope.get('Drag'), $gvars.$['$[]']("DragEvent"), $scope.get('Gamepad'), $gvars.$['$[]']("GamepadEvent"), $scope.get('HashChange'), $gvars.$['$[]']("HashChangeEvent"), $scope.get('Progress'), $gvars.$['$[]']("ProgressEvent"), $scope.get('PageTransition'), $gvars.$['$[]']("PageTransitionEvent"), $scope.get('PopState'), $gvars.$['$[]']("PopStateEvent"), $scope.get('Storage'), $gvars.$['$[]']("StorageEvent"), $scope.get('Touch'), $gvars.$['$[]']("TouchEvent"), $scope.get('Sensor'), $gvars.$['$[]']("SensorEvent"), $scope.get('Mouse'), $gvars.$['$[]']("MouseEvent"), $scope.get('Keyboard'), $gvars.$['$[]']("KeyboardEvent"), $scope.get('Focus'), $gvars.$['$[]']("FocusEvent"), $scope.get('Wheel'), $gvars.$['$[]']("WheelEvent"), $scope.get('Custom'), $gvars.$['$[]']("CustomEvent")));
        });

        Opal.defs(self, '$class_for', function(name) {
          var $a, $b, self = this, type = nil, $case = nil;

          type = (function() {$case = self.$name_for(name);if ("animationend"['$===']($case) || "animationiteration"['$===']($case) || "animationstart"['$===']($case)) {return $scope.get('Animation')}else if ("audioprocess"['$===']($case)) {return $scope.get('AudioProcessing')}else if ("beforeunload"['$===']($case)) {return $scope.get('BeforeUnload')}else if ("compositionend"['$===']($case) || "compositionstart"['$===']($case) || "compositionupdate"['$===']($case)) {return $scope.get('Composition')}else if ("copy"['$===']($case) || "cut"['$===']($case)) {return $scope.get('Clipboard')}else if ("devicelight"['$===']($case)) {return $scope.get('DeviceLight')}else if ("devicemotion"['$===']($case)) {return $scope.get('DeviceMotion')}else if ("deviceorientation"['$===']($case)) {return $scope.get('DeviceOrientation')}else if ("deviceproximity"['$===']($case)) {return $scope.get('DeviceProximity')}else if ("drag"['$===']($case) || "dragend"['$===']($case) || "dragleave"['$===']($case) || "dragover"['$===']($case) || "dragstart"['$===']($case) || "drop"['$===']($case)) {return $scope.get('Drag')}else if ("gamepadconnected"['$===']($case) || "gamepaddisconnected"['$===']($case)) {return $scope.get('Gamepad')}else if ("hashchange"['$===']($case)) {return $scope.get('HashChange')}else if ("load"['$===']($case) || "loadend"['$===']($case) || "loadstart"['$===']($case)) {return $scope.get('Progress')}else if ("pagehide"['$===']($case) || "pageshow"['$===']($case)) {return $scope.get('PageTransition')}else if ("popstate"['$===']($case)) {return $scope.get('PopState')}else if ("storage"['$===']($case)) {return $scope.get('Storage')}else if ("touchcancel"['$===']($case) || "touchend"['$===']($case) || "touchleave"['$===']($case) || "touchmove"['$===']($case) || "touchstart"['$===']($case)) {return $scope.get('Touch')}else if ("compassneedscalibration"['$===']($case) || "userproximity"['$===']($case)) {return $scope.get('Sensor')}else if ("message"['$===']($case)) {return $scope.get('Message')}else if ("close"['$===']($case)) {return $scope.get('Close')}else if ("click"['$===']($case) || "contextmenu"['$===']($case) || "dblclick"['$===']($case) || "mousedown"['$===']($case) || "mouseenter"['$===']($case) || "mouseleave"['$===']($case) || "mousemove"['$===']($case) || "mouseout"['$===']($case) || "mouseover"['$===']($case) || "mouseup"['$===']($case) || "show"['$===']($case)) {return $scope.get('Mouse')}else if ("keydown"['$===']($case) || "keypress"['$===']($case) || "keyup"['$===']($case)) {return $scope.get('Keyboard')}else if ("blur"['$===']($case) || "focus"['$===']($case) || "focusin"['$===']($case) || "focusout"['$===']($case)) {return $scope.get('Focus')}else if ("wheel"['$===']($case)) {return $scope.get('Wheel')}else if ("abort"['$===']($case) || "afterprint"['$===']($case) || "beforeprint"['$===']($case) || "cached"['$===']($case) || "canplay"['$===']($case) || "canplaythrough"['$===']($case) || "change"['$===']($case) || "chargingchange"['$===']($case) || "chargingtimechange"['$===']($case) || "checking"['$===']($case) || "close"['$===']($case) || "dischargingtimechange"['$===']($case) || "DOMContentLoaded"['$===']($case) || "downloading"['$===']($case) || "durationchange"['$===']($case) || "emptied"['$===']($case) || "ended"['$===']($case) || "error"['$===']($case) || "fullscreenchange"['$===']($case) || "fullscreenerror"['$===']($case) || "input"['$===']($case) || "invalid"['$===']($case) || "levelchange"['$===']($case) || "loadeddata"['$===']($case) || "loadedmetadata"['$===']($case) || "noupdate"['$===']($case) || "obsolete"['$===']($case) || "offline"['$===']($case) || "online"['$===']($case) || "open"['$===']($case) || "orientationchange"['$===']($case) || "pause"['$===']($case) || "pointerlockchange"['$===']($case) || "pointerlockerror"['$===']($case) || "play"['$===']($case) || "playing"['$===']($case) || "ratechange"['$===']($case) || "readystatechange"['$===']($case) || "reset"['$===']($case) || "seeked"['$===']($case) || "seeking"['$===']($case) || "stalled"['$===']($case) || "submit"['$===']($case) || "success"['$===']($case) || "suspend"['$===']($case) || "timeupdate"['$===']($case) || "updateready"['$===']($case) || "visibilitychange"['$===']($case) || "volumechange"['$===']($case) || "waiting"['$===']($case)) {return $scope.get('Event')}else {return $scope.get('Custom')}})();
          if ((($a = ($b = type['$==']($scope.get('Event'))['$!'](), $b !== false && $b !== nil ?type['$supported?']() : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
            return type
            } else {
            return $scope.get('Event')
          };
        });

        Opal.defs(self, '$create', TMP_2 = function(name, args) {
          var $a, $b, $c, $d, self = this, $iter = TMP_2.$$p, block = $iter || nil, klass = nil, event = nil;

          args = $slice.call(arguments, 1);
          TMP_2.$$p = null;
          name = self.$name_for(name);
          klass = self.$class_for(name);
          event = (function() {if (klass['$=='](self)) {
            return self.$new(new window.Event(name, ($a = ($b = $scope.get('Definition')).$new, $a.$$p = block.$to_proc(), $a).call($b)))
            } else {
            return ($a = ($c = klass).$create, $a.$$p = block.$to_proc(), $a).call($c, name)
          }; return nil; })();
          (($a = [args]), $d = event, $d['$arguments='].apply($d, $a), $a[$a.length-1]);
          return event;
        });

        Opal.defs(self, '$new', TMP_4 = function(value, args) {
          var $a, $b, $c, TMP_3, self = this, $iter = TMP_4.$$p, $yield = $iter || nil, klass = nil;

          args = $slice.call(arguments, 1);
          TMP_4.$$p = null;
          $a = Opal.to_ary(($b = ($c = self.$classes()).$find, $b.$$p = (TMP_3 = function(_, constructor){var self = TMP_3.$$s || this;
if (_ == null) _ = nil;if (constructor == null) constructor = nil;
          return $scope.get('Native')['$is_a?'](value, constructor)}, TMP_3.$$s = self, TMP_3), $b).call($c)), klass = ($a[0] == null ? nil : $a[0]), _ = ($a[1] == null ? nil : $a[1]);
          if ((($a = ((($b = klass['$!']()) !== false && $b !== nil) ? $b : klass['$=='](self))) !== nil && (!$a.$$is_boolean || $a == true))) {
            return Opal.find_super_dispatcher(self, 'new', TMP_4, null, $Event).apply(self, [value].concat(args))
            } else {
            return ($a = klass).$new.apply($a, [value].concat(args))
          };
        });

        self.$attr_reader("target", "callback");

        def.$initialize = TMP_5 = function(native$, callback) {
          var $a, self = this, $iter = TMP_5.$$p, $yield = $iter || nil;

          if (callback == null) {
            callback = nil
          }
          TMP_5.$$p = null;
          Opal.find_super_dispatcher(self, 'initialize', TMP_5, null).apply(self, [native$]);
          self.target = $scope.get('Target').$convert(self["native"].target);
          return $a = Opal.to_ary(callback), self.callback = ($a[0] == null ? nil : $a[0]);
        };

        def.$off = function() {
          var $a, self = this;

          if ((($a = self.callback) !== nil && (!$a.$$is_boolean || $a == true))) {
            return self.callback.$off()
            } else {
            return nil
          };
        };

        def.$arguments = function() {
          var self = this;

          return self["native"].arguments || [];
        };

        def['$arguments='] = function(args) {
          var self = this;

          return self["native"].arguments = args;
        };

        self.$alias_native("bubbles?", "bubbles");

        self.$alias_native("cancelable?", "cancelable");

        self.$alias_native("name", "type");

        self.$alias_native("data");

        self.$alias_native("phase", "eventPhase");

        self.$alias_native("at", "timeStamp");

        def['$stopped?'] = function() {
          var self = this;

          return !!self["native"].stopped;
        };

        return (def['$stop!'] = function() {
          var $a, self = this;

          if ((($a = (typeof(self["native"].stopPropagation) !== "undefined")) !== nil && (!$a.$$is_boolean || $a == true))) {
            self["native"].stopPropagation();};
          if ((($a = (typeof(self["native"].preventDefault) !== "undefined")) !== nil && (!$a.$$is_boolean || $a == true))) {
            self["native"].preventDefault();};
          return self["native"].stopped = true;
        }, nil) && 'stop!';
      })(self, null)
    })(self)
  })(self);
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/node_set"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass, $range = Opal.range;

  Opal.add_stubs(['$attr_reader', '$each', '$===', '$concat', '$to_a', '$push', '$DOM', '$convert', '$respond_to?', '$__send__', '$to_proc', '$new', '$document', '$dup', '$to_ary', '$select', '$matches?', '$after', '$last', '$raise', '$before', '$first', '$children', '$uniq', '$flatten', '$map', '$search', '$[]', '$inspect']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $NodeSet(){};
        var self = $NodeSet = $klass($base, $super, 'NodeSet', $NodeSet);

        var def = self.$$proto, $scope = self.$$scope, TMP_2;

        def.literal = def.document = nil;
        self.$attr_reader("document");

        def.$initialize = function(document, list) {
          var $a, $b, TMP_1, self = this;

          if (list == null) {
            list = []
          }
          self.document = document;
          self.literal = [];
          return ($a = ($b = list).$each, $a.$$p = (TMP_1 = function(el){var self = TMP_1.$$s || this, $a;
            if (self.literal == null) self.literal = nil;
if (el == null) el = nil;
          if ((($a = $scope.get('NodeSet')['$==='](el)) !== nil && (!$a.$$is_boolean || $a == true))) {
              return self.literal.$concat(el.$to_a())
              } else {
              return self.literal.$push(self.$DOM($scope.get('Native').$convert(el)))
            }}, TMP_1.$$s = self, TMP_1), $a).call($b);
        };

        def.$method_missing = TMP_2 = function(name, args) {
          var $a, $b, TMP_3, $c, self = this, $iter = TMP_2.$$p, block = $iter || nil, result = nil;

          args = $slice.call(arguments, 1);
          TMP_2.$$p = null;
          if ((($a = self.literal['$respond_to?'](name)) !== nil && (!$a.$$is_boolean || $a == true))) {
            } else {
            ($a = ($b = self).$each, $a.$$p = (TMP_3 = function(el){var self = TMP_3.$$s || this, $a, $b;
if (el == null) el = nil;
            return ($a = ($b = el).$__send__, $a.$$p = block.$to_proc(), $a).apply($b, [name].concat(args))}, TMP_3.$$s = self, TMP_3), $a).call($b);
            return self;
          };
          result = ($a = ($c = self.literal).$__send__, $a.$$p = block.$to_proc(), $a).apply($c, [name].concat(args));
          if ((($a = result === self.literal) !== nil && (!$a.$$is_boolean || $a == true))) {
            return self
          } else if ((($a = $scope.get('Array')['$==='](result)) !== nil && (!$a.$$is_boolean || $a == true))) {
            return $scope.get('NodeSet').$new(self.document, result)
            } else {
            return result
          };
        };

        def.$dup = function() {
          var self = this;

          return $scope.get('NodeSet').$new(self.$document(), self.$to_ary().$dup());
        };

        def.$filter = function(expression) {
          var $a, $b, TMP_4, self = this;

          return $scope.get('NodeSet').$new(self.$document(), ($a = ($b = self.literal).$select, $a.$$p = (TMP_4 = function(node){var self = TMP_4.$$s || this;
if (node == null) node = nil;
          return node['$matches?'](expression)}, TMP_4.$$s = self, TMP_4), $a).call($b));
        };

        def.$after = function(node) {
          var self = this;

          return self.$last().$after(node);
        };

        def.$at = function(path) {
          var self = this;

          return self.$raise($scope.get('NotImplementedError'));
        };

        def.$at_css = function(rules) {
          var self = this;

          rules = $slice.call(arguments, 0);
          return self.$raise($scope.get('NotImplementedError'));
        };

        def.$at_xpath = function(paths) {
          var self = this;

          paths = $slice.call(arguments, 0);
          return self.$raise($scope.get('NotImplementedError'));
        };

        def.$before = function() {
          var self = this;

          return self.$first().$before();
        };

        def.$children = function() {
          var $a, $b, TMP_5, self = this, result = nil;

          result = $scope.get('NodeSet').$new(self.$document());
          ($a = ($b = self).$each, $a.$$p = (TMP_5 = function(n){var self = TMP_5.$$s || this;
if (n == null) n = nil;
          return result.$concat(n.$children())}, TMP_5.$$s = self, TMP_5), $a).call($b);
          return result;
        };

        def.$css = function(paths) {
          var self = this;

          paths = $slice.call(arguments, 0);
          return self.$raise($scope.get('NotImplementedError'));
        };

        def.$search = function(what) {
          var $a, $b, TMP_6, self = this;

          what = $slice.call(arguments, 0);
          return ($a = ($b = self).$map, $a.$$p = (TMP_6 = function(n){var self = TMP_6.$$s || this, $a;
if (n == null) n = nil;
          return ($a = n).$search.apply($a, [].concat(what))}, TMP_6.$$s = self, TMP_6), $a).call($b).$flatten().$uniq();
        };

        return (def.$inspect = function() {
          var self = this;

          return "#<DOM::NodeSet: " + (self.literal.$inspect()['$[]']($range(1, -2, false)));
        }, nil) && 'inspect';
      })(self, null)
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/node"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass, $hash2 = Opal.hash2;

  Opal.add_stubs(['$include', '$==', '$[]', '$new', '$raise', '$try_convert', '$downcase', '$name', '$add_child', '$native?', '$respond_to?', '$each', '$===', '$convert', '$parent', '$document', '$last', '$<<', '$pop', '$select', '$matches?', '$detach', '$clear', '$remove_child', '$to_proc', '$children', '$node_type', '$first', '$DOM', '$element_children', '$to_s', '$next', '$!', '$element?', '$previous']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Node(){};
        var self = $Node = $klass($base, $super, 'Node', $Node);

        var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_4;

        def["native"] = nil;
        self.$include($scope.get('Native'));

        Opal.cdecl($scope, 'ELEMENT_NODE', 1);

        Opal.cdecl($scope, 'ATTRIBUTE_NODE', 2);

        Opal.cdecl($scope, 'TEXT_NODE', 3);

        Opal.cdecl($scope, 'CDATA_SECTION_NODE', 4);

        Opal.cdecl($scope, 'ENTITY_REFERENCE_NOCE', 5);

        Opal.cdecl($scope, 'ENTITY_NODE', 6);

        Opal.cdecl($scope, 'PROCESSING_INSTRUCTION_NODE', 7);

        Opal.cdecl($scope, 'COMMENT_NODE', 8);

        Opal.cdecl($scope, 'DOCUMENT_NODE', 9);

        Opal.cdecl($scope, 'DOCUMENT_TYPE_NODE', 10);

        Opal.cdecl($scope, 'DOCUMENT_FRAGMENT_NODE', 11);

        Opal.cdecl($scope, 'NOTATION_NODE', 12);

        Opal.defs(self, '$new', TMP_1 = function(value) {var $zuper = $slice.call(arguments, 0);
          var $a, self = this, $iter = TMP_1.$$p, $yield = $iter || nil, klass = nil;
          if (self.classes == null) self.classes = nil;

          TMP_1.$$p = null;
          if (self['$==']($scope.get('Node'))) {
            ((($a = self.classes) !== false && $a !== nil) ? $a : self.classes = [nil, $scope.get('Element'), $scope.get('Attribute'), $scope.get('Text'), $scope.get('CDATA'), nil, nil, nil, $scope.get('Comment'), $scope.get('Document'), nil, $scope.get('DocumentFragment')]);
            if ((($a = klass = self.classes['$[]'](value.nodeType)) !== nil && (!$a.$$is_boolean || $a == true))) {
              return klass.$new(value)
              } else {
              return self.$raise($scope.get('ArgumentError'), "cannot instantiate a non derived Node object")
            };
            } else {
            return Opal.find_super_dispatcher(self, 'new', TMP_1, $iter, $Node).apply(self, $zuper)
          };
        });

        def['$=='] = function(other) {
          var self = this;

          return self["native"] === $scope.get('Native').$try_convert(other);
        };

        def['$=~'] = function(name) {
          var self = this;

          return self.$name().$downcase()['$=='](name.$downcase());
        };

        def['$<<'] = function(node) {
          var self = this;

          return self.$add_child(node);
        };

        def['$<=>'] = function(other) {
          var self = this;

          return self.$raise($scope.get('NotImplementedError'));
        };

        def.$add_child = function(node) {
          var $a, $b, TMP_2, self = this;

          if ((($a = self['$native?'](node)) !== nil && (!$a.$$is_boolean || $a == true))) {
            self["native"].appendChild(node);
          } else if ((($a = node['$respond_to?']("each")) !== nil && (!$a.$$is_boolean || $a == true))) {
            ($a = ($b = node).$each, $a.$$p = (TMP_2 = function(n){var self = TMP_2.$$s || this;
if (n == null) n = nil;
            return self.$add_child(n)}, TMP_2.$$s = self, TMP_2), $a).call($b)
          } else if ((($a = $scope.get('String')['$==='](node)) !== nil && (!$a.$$is_boolean || $a == true))) {
            self["native"].appendChild(self["native"].ownerDocument.createTextNode(node));
            } else {
            self["native"].appendChild($scope.get('Native').$convert(node));
          };
          return self;
        };

        def.$add_next_sibling = function(node) {
          var self = this;

          self["native"].parentNode.insertBefore(node, self["native"].nextSibling);
          return self;
        };

        def.$add_previous_sibling = function(node) {
          var self = this;

          self["native"].parentNode.insertBefore(node, self["native"]);
          return self;
        };

        Opal.defn(self, '$after', def.$add_next_sibling);

        def.$append_to = function(element) {
          var self = this;

          element.$add_child(self);
          return self;
        };

        def.$ancestors = function(expression) {
          var $a, $b, TMP_3, self = this, parents = nil, parent = nil;

          if (expression == null) {
            expression = nil
          }
          if ((($a = self.$parent()) !== nil && (!$a.$$is_boolean || $a == true))) {
            } else {
            return $scope.get('NodeSet').$new(self.$document())
          };
          parents = [self.$parent()];
          while ((($b = parent = parents.$last().$parent()) !== nil && (!$b.$$is_boolean || $b == true))) {
          parents['$<<'](parent)};
          if ((($a = $scope.get('Document')['$==='](parents.$last())) !== nil && (!$a.$$is_boolean || $a == true))) {
            parents.$pop()};
          if (expression !== false && expression !== nil) {
            } else {
            return $scope.get('NodeSet').$new(self.$document(), parents)
          };
          return $scope.get('NodeSet').$new(self.$document(), ($a = ($b = parents).$select, $a.$$p = (TMP_3 = function(p){var self = TMP_3.$$s || this;
if (p == null) p = nil;
          return p['$matches?'](expression)}, TMP_3.$$s = self, TMP_3), $a).call($b));
        };

        Opal.defn(self, '$before', def.$add_previous_sibling);

        def.$remove = function() {
          var self = this;

          self.$detach();
          self.$clear();
          return self;
        };

        def.$detach = function() {
          var $a, self = this;

          if ((($a = self.$parent()) !== nil && (!$a.$$is_boolean || $a == true))) {
            self.$parent().$remove_child(self)};
          return self;
        };

        def.$clear = function() {
          var self = this;

          return nil;
        };

        def.$remove_child = function(element) {
          var self = this;

          self["native"].removeChild($scope.get('Native').$try_convert(element));
          return self;
        };

        def.$clear = function() {
          var $a, $b, self = this;

          return ($a = ($b = self.$children()).$each, $a.$$p = "remove".$to_proc(), $a).call($b);
        };

        def['$blank?'] = function() {
          var self = this;

          return self.$raise($scope.get('NotImplementedError'));
        };

        def['$cdata?'] = function() {
          var self = this;

          return self.$node_type()['$==']($scope.get('CDATA_SECTION_NODE'));
        };

        def.$child = function() {
          var self = this;

          return self.$children().$first();
        };

        def.$children = function() {
          var self = this;

          return $scope.get('NodeSet').$new(self.$document(), (($scope.get('Native')).$$scope.get('Array')).$new(self["native"].childNodes));
        };

        def['$children='] = function(node) {
          var self = this;

          return self.$raise($scope.get('NotImplementedError'));
        };

        def['$comment?'] = function() {
          var self = this;

          return self.$node_type()['$==']($scope.get('COMMENT_NODE'));
        };

        def.$document = function() {
          var self = this;

          return self.$DOM(self["native"].ownerDocument);
        };

        def['$document?'] = function() {
          var self = this;

          return self.$node_type()['$==']($scope.get('DOCUMENT_NODE'));
        };

        def['$elem?'] = function() {
          var self = this;

          return self.$node_type()['$==']($scope.get('ELEMENT_NODE'));
        };

        Opal.defn(self, '$element?', def['$elem?']);

        def.$element_children = function() {
          var $a, $b, self = this;

          return ($a = ($b = self.$children()).$select, $a.$$p = "element?".$to_proc(), $a).call($b);
        };

        Opal.defn(self, '$elements', def.$element_children);

        def.$first_element_child = function() {
          var self = this;

          return self.$element_children().$first();
        };

        def['$fragment?'] = function() {
          var self = this;

          return self.$node_type()['$==']($scope.get('DOCUMENT_FRAGMENT_NODE'));
        };

        def.$hash = function() {
          var self = this;

          return nil;
        };

        def.$inner_html = function() {
          var self = this;

          return self["native"].innerHTML;
        };

        def['$inner_html='] = function(value) {
          var self = this;

          return self["native"].innerHTML = value;
        };

        def.$inner_text = function() {
          var self = this;

          return self["native"].textContent;
        };

        Opal.defn(self, '$content', def.$inner_text);

        def['$inner_text='] = function(value) {
          var self = this;

          return self["native"].textContent = value;
        };

        Opal.defn(self, '$content=', def['$inner_text=']);

        def.$last_element_child = function() {
          var self = this;

          return self.$element_children().$last();
        };

        def['$matches?'] = function(expression) {
          var self = this;

          return false;
        };

        def.$name = function() {
          var self = this;

          return self["native"].nodeName || nil;
        };

        def['$name='] = function(value) {
          var self = this;

          return self["native"].nodeName = value.$to_s();
        };

        def.$namespace = function() {
          var self = this;

          return self["native"].namespaceURI || nil;
        };

        def.$next = function() {
          var $a, self = this;

          if ((($a = self["native"].nextSibling != null) !== nil && (!$a.$$is_boolean || $a == true))) {
            return self.$DOM(self["native"].nextSibling)
            } else {
            return nil
          };
        };

        def.$next_element = function() {
          var $a, $b, $c, self = this, current = nil;

          current = self.$next();
          while ((($b = (($c = current !== false && current !== nil) ? current['$element?']()['$!']() : $c)) !== nil && (!$b.$$is_boolean || $b == true))) {
          current = current.$next()};
          return current;
        };

        Opal.defn(self, '$next_sibling', def.$next);

        Opal.defn(self, '$node_name', def.$name);

        Opal.defn(self, '$node_name=', def['$name=']);

        def.$node_type = function() {
          var self = this;

          return self["native"].nodeType;
        };

        def.$parent = function() {
          var $a, self = this;

          if ((($a = self["native"].parentNode != null) !== nil && (!$a.$$is_boolean || $a == true))) {
            return self.$DOM(self["native"].parentNode)
            } else {
            return nil
          };
        };

        def['$parent='] = function(node) {
          var self = this;

          return self["native"].parentNode = $scope.get('Native').$try_convert(node);
        };

        def.$parse = function(text, options) {
          var self = this;

          if (options == null) {
            options = $hash2([], {})
          }
          return self.$raise($scope.get('NotImplementedError'));
        };

        def.$path = function() {
          var self = this;

          return self.$raise($scope.get('NotImplementedError'));
        };

        def.$previous = function() {
          var $a, self = this;

          if ((($a = self["native"].previousSibling != null) !== nil && (!$a.$$is_boolean || $a == true))) {
            return self.$DOM(self["native"].previousSibling)
            } else {
            return nil
          };
        };

        Opal.defn(self, '$previous=', def.$add_previous_sibling);

        def.$previous_element = function() {
          var $a, $b, $c, self = this, current = nil;

          current = self.$previous();
          while ((($b = (($c = current !== false && current !== nil) ? current['$element?']()['$!']() : $c)) !== nil && (!$b.$$is_boolean || $b == true))) {
          current = current.$previous()};
          return current;
        };

        Opal.defn(self, '$previous_sibling', def.$previous);

        def.$replace = function(node) {
          var self = this;

          self["native"].parentNode.replaceChild(self["native"], $scope.get('Native').$try_convert(node));
          return node;
        };

        Opal.defn(self, '$text', def.$inner_text);

        Opal.defn(self, '$text=', def['$inner_text=']);

        def['$text?'] = function() {
          var self = this;

          return self.$node_type()['$==']($scope.get('TEXT_NODE'));
        };

        def.$traverse = TMP_4 = function() {
          var self = this, $iter = TMP_4.$$p, block = $iter || nil;

          TMP_4.$$p = null;
          return self.$raise($scope.get('NotImplementedError'));
        };

        Opal.defn(self, '$type', def.$node_type);

        def.$value = function() {
          var self = this;

          return self["native"].nodeValue || nil;
        };

        def['$value='] = function(value) {
          var self = this;

          return self["native"].nodeValue = value;
        };

        return (def.$inspect = function() {
          var self = this;

          return "#<DOM::Node: " + (self.$name()) + ">";
        }, nil) && 'inspect';
      })(self, null)
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/attribute"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass;

  Opal.add_stubs(['$include']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Attribute(){};
        var self = $Attribute = $klass($base, $super, 'Attribute', $Attribute);

        var def = self.$$proto, $scope = self.$$scope;

        def["native"] = nil;
        self.$include($scope.get('Native'));

        def['$id?'] = function() {
          var self = this;

          return self["native"].isId;
        };

        def.$name = function() {
          var self = this;

          return self["native"].name;
        };

        return (def.$value = function() {
          var self = this;

          return self["native"].value;
        }, nil) && 'value';
      })(self, null)
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/character_data"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass;

  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $CharacterData(){};
        var self = $CharacterData = $klass($base, $super, 'CharacterData', $CharacterData);

        var def = self.$$proto, $scope = self.$$scope;

        def["native"] = nil;
        def.$data = function() {
          var self = this;

          return self["native"].data;
        };

        def.$append = function(string) {
          var self = this;

          self["native"].appendData(string);
          return self;
        };

        def.$insert = function(string, offset) {
          var self = this;

          if (offset == null) {
            offset = 0
          }
          self["native"].insertData(offset, string);
          return self;
        };

        def.$delete = function(count, offset) {
          var self = this;

          if (offset == null) {
            offset = 0
          }
          self["native"].deleteData(offset, count);
          return self;
        };

        def.$replace = function(string, offset, count) {
          var self = this;

          if (offset == null) {
            offset = 0
          }
          if (count == null) {
            count = self["native"].length
          }
          self["native"].replaceData(offset, count, string);
          return self;
        };

        return (def.$substring = function(count, offset) {
          var self = this;

          if (offset == null) {
            offset = 0
          }
          return self["native"].substringData(offset, count);
        }, nil) && 'substring';
      })(self, $scope.get('Node'))
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/text"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass, $gvars = Opal.gvars;

  Opal.add_stubs(['$create_text', '$data']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Text(){};
        var self = $Text = $klass($base, $super, 'Text', $Text);

        var def = self.$$proto, $scope = self.$$scope;

        def["native"] = nil;
        Opal.defs(self, '$create', function(args) {
          var $a, self = this;
          if ($gvars.document == null) $gvars.document = nil;

          args = $slice.call(arguments, 0);
          return ($a = $gvars.document).$create_text.apply($a, [].concat(args));
        });

        def.$whole = function() {
          var self = this;

          return self["native"].wholeText;
        };

        def.$split = function(offset) {
          var self = this;

          return self["native"].splitText(offset);
        };

        return (def.$inspect = function() {
          var self = this;

          return "#<DOM::Text: " + (self.$data()) + ">";
        }, nil) && 'inspect';
      })(self, $scope.get('CharacterData'))
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/cdata"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass;

  Opal.add_stubs(['$value']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $CDATA(){};
        var self = $CDATA = $klass($base, $super, 'CDATA', $CDATA);

        var def = self.$$proto, $scope = self.$$scope;

        return (def.$inspect = function() {
          var self = this;

          return "#<DOM::CDATA: " + (self.$value()) + ">";
        }, nil) && 'inspect'
      })(self, $scope.get('Text'))
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/comment"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass;

  Opal.add_stubs(['$value']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Comment(){};
        var self = $Comment = $klass($base, $super, 'Comment', $Comment);

        var def = self.$$proto, $scope = self.$$scope;

        return (def.$inspect = function() {
          var self = this;

          return "#<DOM::Comment: " + (self.$value()) + ">";
        }, nil) && 'inspect'
      })(self, $scope.get('CharacterData'))
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/element/position"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass;

  Opal.add_stubs(['$to_n', '$offset', '$get', '$parent', '$new', '$==', '$[]', '$style', '$=~', '$x=', '$+', '$x', '$to_i', '$y=', '$y', '$-']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Element(){};
        var self = $Element = $klass($base, $super, 'Element', $Element);

        var def = self.$$proto, $scope = self.$$scope;

        return (function($base, $super) {
          function $Position(){};
          var self = $Position = $klass($base, $super, 'Position', $Position);

          var def = self.$$proto, $scope = self.$$scope;

          def.element = nil;
          def.$initialize = function(element) {
            var self = this;

            self.element = element;
            return self["native"] = element.$to_n();
          };

          def.$get = function() {
            var $a, self = this, offset = nil, position = nil, parent = nil, parent_offset = nil;

            offset = self.element.$offset();
            position = offset.$get();
            parent = offset.$parent();
            parent_offset = (($scope.get('Browser')).$$scope.get('Position')).$new(0, 0);
            if (self.element.$style()['$[]']("position")['$==']("fixed")) {
              if ((($a = parent['$=~']("html")) !== nil && (!$a.$$is_boolean || $a == true))) {
                } else {
                parent_offset = parent.$offset()
              };
              ($a = parent_offset, $a['$x=']($a.$x()['$+'](parent.$style()['$[]']("border-top-width").$to_i())));
              ($a = parent_offset, $a['$y=']($a.$y()['$+'](parent.$style()['$[]']("border-left-width").$to_i())));};
            return (($scope.get('Browser')).$$scope.get('Position')).$new(position.$x()['$-'](parent_offset.$x())['$-'](self.element.$style()['$[]']("margin-left").$to_i()), position.$y()['$-'](parent_offset.$y())['$-'](self.element.$style()['$[]']("margin-top").$to_i()));
          };

          def.$x = function() {
            var self = this;

            return self.$get().$x();
          };

          return (def.$y = function() {
            var self = this;

            return self.$get().$y();
          }, nil) && 'y';
        })(self, null)
      })(self, $scope.get('Node'))
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/element/offset"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass;

  Opal.add_stubs(['$to_n', '$DOM', '$root', '$document', '$x', '$get', '$set', '$y', '$window', '$new', '$[]', '$style!', '$==', '$[]=', '$style', '$to_u', '$===', '$first', '$+', '$-', '$px']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Element(){};
        var self = $Element = $klass($base, $super, 'Element', $Element);

        var def = self.$$proto, $scope = self.$$scope;

        return (function($base, $super) {
          function $Offset(){};
          var self = $Offset = $klass($base, $super, 'Offset', $Offset);

          var def = self.$$proto, $scope = self.$$scope;

          def["native"] = def.element = nil;
          def.$initialize = function(element) {
            var self = this;

            self.element = element;
            return self["native"] = element.$to_n();
          };

          def.$parent = function() {
            var self = this;

            return self.$DOM(self["native"].offsetParent || self.element.$document().$root().$to_n());
          };

          def.$x = function() {
            var self = this;

            return self.$get().$x();
          };

          def['$x='] = function(value) {
            var self = this;

            return self.$set(value, nil);
          };

          def.$y = function() {
            var self = this;

            return self.$get().$y();
          };

          def['$y='] = function(value) {
            var self = this;

            return self.$set(nil, value);
          };

          def.$get = function() {
            var self = this, doc = nil, root = nil, win = nil;

            doc = self.element.$document();
            root = doc.$root().$to_n();
            win = doc.$window().$to_n();
            
      var box = self["native"].getBoundingClientRect(),
          y   = box.top + (win.pageYOffset || root.scrollTop) - (root.clientTop || 0),
          x   = box.left + (win.pageXOffset || root.scrollLeft) - (root.clientLeft || 0);
    ;
            return (($scope.get('Browser')).$$scope.get('Position')).$new(x, y);
          };

          return (def.$set = function(value) {
            var $a, self = this, position = nil, offset = nil, top = nil, left = nil, x = nil, y = nil;

            value = $slice.call(arguments, 0);
            position = self.element['$style!']()['$[]']("position");
            if (position['$==']("static")) {
              self.element.$style()['$[]=']("position", "relative")};
            offset = self.$get();
            top = self.element['$style!']()['$[]']("top").$to_u();
            left = self.element['$style!']()['$[]']("left").$to_u();
            if ((($a = (($scope.get('Browser')).$$scope.get('Position'))['$==='](value.$first())) !== nil && (!$a.$$is_boolean || $a == true))) {
              $a = [value.$first().$x(), value.$first().$y()], x = $a[0], y = $a[1]
            } else if ((($a = $scope.get('Hash')['$==='](value.$first())) !== nil && (!$a.$$is_boolean || $a == true))) {
              $a = [value.$first()['$[]']("x"), value.$first()['$[]']("y")], x = $a[0], y = $a[1]
              } else {
              $a = Opal.to_ary(value), x = ($a[0] == null ? nil : $a[0]), y = ($a[1] == null ? nil : $a[1])
            };
            if (x !== false && x !== nil) {
              self.element.$style()['$[]=']("left", (x.$px()['$-'](offset.$x()))['$+'](left))};
            if (y !== false && y !== nil) {
              return self.element.$style()['$[]=']("top", (y.$px()['$-'](offset.$y()))['$+'](top))
              } else {
              return nil
            };
          }, nil) && 'set';
        })(self, null)
      })(self, $scope.get('Node'))
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/element/scroll"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass;

  Opal.add_stubs(['$to_n', '$new', '$x', '$position', '$y', '$[]']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Element(){};
        var self = $Element = $klass($base, $super, 'Element', $Element);

        var def = self.$$proto, $scope = self.$$scope;

        return (function($base, $super) {
          function $Scroll(){};
          var self = $Scroll = $klass($base, $super, 'Scroll', $Scroll);

          var def = self.$$proto, $scope = self.$$scope;

          def["native"] = nil;
          def.$initialize = function(element) {
            var self = this;

            self.element = element;
            return self["native"] = element.$to_n();
          };

          def.$position = function() {
            var self = this;

            return (($scope.get('Browser')).$$scope.get('Position')).$new(self["native"].scrollLeft, self["native"].scrollTop);
          };

          def.$x = function() {
            var self = this;

            return self.$position().$x();
          };

          def.$y = function() {
            var self = this;

            return self.$position().$y();
          };

          def.$to = function(what) {
            var $a, self = this, x = nil, y = nil;

            x = ((($a = what['$[]']("x")) !== false && $a !== nil) ? $a : self.$x());
            y = ((($a = what['$[]']("y")) !== false && $a !== nil) ? $a : self.$y());
            self["native"].scrollTop = y;
            self["native"].scrollLeft = x;
            return self;
          };

          def.$height = function() {
            var self = this;

            return self["native"].scrollHeight;
          };

          def.$width = function() {
            var self = this;

            return self["native"].scrollWidth;
          };

          return (def.$by = function(what) {
            var $a, self = this, x = nil, y = nil;

            x = ((($a = what['$[]']("x")) !== false && $a !== nil) ? $a : 0);
            y = ((($a = what['$[]']("y")) !== false && $a !== nil) ? $a : 0);
            self["native"].scrollBy(x, y);
            return self;
          }, nil) && 'by';
        })(self, null)
      })(self, $scope.get('Node'))
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/element/input"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass;

  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Element(){};
        var self = $Element = $klass($base, $super, 'Element', $Element);

        var def = self.$$proto, $scope = self.$$scope;

        return (function($base, $super) {
          function $Input(){};
          var self = $Input = $klass($base, $super, 'Input', $Input);

          var def = self.$$proto, $scope = self.$$scope;

          def["native"] = nil;
          def.$value = function() {
            var self = this;

            return self["native"].value;
          };

          def['$value='] = function(value) {
            var self = this;

            return self["native"].value = value;
          };

          return (def.$clear = function() {
            var self = this;

            return self["native"].value = '';
          }, nil) && 'clear';
        })(self, $scope.get('Element'))
      })(self, $scope.get('Node'))
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/element"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass, $gvars = Opal.gvars, $hash2 = Opal.hash2;

  Opal.add_stubs(['$require', '$create_element', '$==', '$downcase', '$===', '$new', '$include', '$target', '$DOM', '$alias_native', '$join', '$uniq', '$+', '$class_names', '$-', '$reject', '$to_proc', '$split', '$[]', '$to_s', '$!', '$attributes_nodesmap', '$map', '$attribute_nodes', '$empty?', '$set', '$offset', '$document', '$clear', '$<<', '$flatten', '$xpath', '$first', '$css', '$each', '$concat', '$to_a', '$is_a?', '$replace', '$assign', '$apply', '$to_n', '$window', '$name', '$attr_reader', '$enum_for', '$value', '$get_attribute', '$set_attribute', '$[]=']);
  self.$require("browser/dom/element/position");
  self.$require("browser/dom/element/offset");
  self.$require("browser/dom/element/scroll");
  self.$require("browser/dom/element/input");
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Element(){};
        var self = $Element = $klass($base, $super, 'Element', $Element);

        var def = self.$$proto, $scope = self.$$scope, TMP_1, $a, $b, TMP_2, TMP_4, TMP_9;

        def["native"] = nil;
        Opal.defs(self, '$create', function(args) {
          var $a, self = this;
          if ($gvars.document == null) $gvars.document = nil;

          args = $slice.call(arguments, 0);
          return ($a = $gvars.document).$create_element.apply($a, [].concat(args));
        });

        Opal.defs(self, '$new', TMP_1 = function(node) {var $zuper = $slice.call(arguments, 0);
          var self = this, $iter = TMP_1.$$p, $yield = $iter || nil, $case = nil;

          TMP_1.$$p = null;
          if (self['$==']($scope.get('Element'))) {
            return (function() {$case = (node.nodeName).$downcase();if ("input"['$===']($case)) {return $scope.get('Input').$new(node)}else {return Opal.find_super_dispatcher(self, 'new', TMP_1, $iter, $Element).apply(self, $zuper)}})()
            } else {
            return Opal.find_super_dispatcher(self, 'new', TMP_1, $iter, $Element).apply(self, $zuper)
          };
        });

        self.$include((($scope.get('Event')).$$scope.get('Target')));

        ($a = ($b = self).$target, $a.$$p = (TMP_2 = function(value){var self = TMP_2.$$s || this;
if (value == null) value = nil;
        try {return self.$DOM(value) } catch ($err) { return nil }}, TMP_2.$$s = self, TMP_2), $a).call($b);

        self.$alias_native("id");

        def.$add_class = function(names) {
          var self = this;

          names = $slice.call(arguments, 0);
          self["native"].className = (self.$class_names()['$+'](names)).$uniq().$join(" ");
          return self;
        };

        def.$remove_class = function(names) {
          var self = this;

          names = $slice.call(arguments, 0);
          self["native"].className = (self.$class_names()['$-'](names)).$join(" ");
          return self;
        };

        self.$alias_native("class_name", "className");

        def.$class_names = function() {
          var $a, $b, self = this;

          return ($a = ($b = (self["native"].className).$split(/\s+/)).$reject, $a.$$p = "empty?".$to_proc(), $a).call($b);
        };

        Opal.defn(self, '$attribute', def.$attr);

        def.$attribute_nodes = function() {
          var $a, $b, TMP_3, self = this;

          return ($a = ($b = (($scope.get('Native')).$$scope.get('Array'))).$new, $a.$$p = (TMP_3 = function(e){var self = TMP_3.$$s || this;
if (e == null) e = nil;
          return self.$DOM(e)}, TMP_3.$$s = self, TMP_3), $a).call($b, self["native"].attributes, $hash2(["get"], {"get": "item"}));
        };

        def.$attributes = function(options) {
          var self = this;

          if (options == null) {
            options = $hash2([], {})
          }
          return $scope.get('Attributes').$new(self, options);
        };

        def.$get = function(name, options) {
          var $a, self = this, namespace = nil;

          if (options == null) {
            options = $hash2([], {})
          }
          if ((($a = namespace = options['$[]']("namespace")) !== nil && (!$a.$$is_boolean || $a == true))) {
            return self["native"].getAttributeNS(namespace.$to_s(), name.$to_s()) || nil;
            } else {
            return self["native"].getAttribute(name.$to_s()) || nil;
          };
        };

        def.$set = function(name, value, options) {
          var $a, self = this, namespace = nil;

          if (options == null) {
            options = $hash2([], {})
          }
          if ((($a = namespace = options['$[]']("namespace")) !== nil && (!$a.$$is_boolean || $a == true))) {
            return self["native"].setAttributeNS(namespace.$to_s(), name.$to_s(), value);
            } else {
            return self["native"].setAttribute(name.$to_s(), value.$to_s());
          };
        };

        Opal.defn(self, '$[]', def.$get);

        Opal.defn(self, '$[]=', def.$set);

        Opal.defn(self, '$attr', def.$get);

        Opal.defn(self, '$attribute', def.$get);

        Opal.defn(self, '$get_attribute', def.$get);

        Opal.defn(self, '$set_attribute', def.$set);

        def['$key?'] = function(name) {
          var self = this;

          return self['$[]'](name)['$!']()['$!']();
        };

        def.$keys = function() {
          var $a, $b, self = this;

          return ($a = ($b = self).$attributes_nodesmap, $a.$$p = "name".$to_proc(), $a).call($b);
        };

        def.$values = function() {
          var $a, $b, self = this;

          return ($a = ($b = self.$attribute_nodes()).$map, $a.$$p = "value".$to_proc(), $a).call($b);
        };

        def.$remove_attribute = function(name) {
          var self = this;

          return self["native"].removeAttribute(name);
        };

        def.$size = function(inc) {
          var self = this;

          inc = $slice.call(arguments, 0);
          return $scope.get('Size').$new(self["native"].offsetWidth, self["native"].offsetHeight);
        };

        def.$position = function() {
          var self = this;

          return $scope.get('Position').$new(self);
        };

        def.$offset = function(values) {
          var $a, self = this, off = nil;

          values = $slice.call(arguments, 0);
          off = $scope.get('Offset').$new(self);
          if ((($a = values['$empty?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
            } else {
            ($a = off).$set.apply($a, [].concat(values))
          };
          return off;
        };

        def['$offset='] = function(value) {
          var $a, self = this;

          return ($a = self.$offset()).$set.apply($a, [].concat(value));
        };

        def.$scroll = function() {
          var self = this;

          return $scope.get('Scroll').$new(self);
        };

        def.$inner_dom = TMP_4 = function() {
          var $a, $b, self = this, $iter = TMP_4.$$p, block = $iter || nil, doc = nil;

          TMP_4.$$p = null;
          doc = self.$document();
          self.$clear();
          ($a = ($b = $scope.get('Builder')).$new, $a.$$p = block.$to_proc(), $a).call($b, doc, self);
          return self;
        };

        def['$inner_dom='] = function(node) {
          var self = this;

          self.$clear();
          return self['$<<'](node);
        };

        def['$/'] = function(paths) {
          var $a, $b, TMP_5, self = this;

          paths = $slice.call(arguments, 0);
          return ($a = ($b = paths).$map, $a.$$p = (TMP_5 = function(path){var self = TMP_5.$$s || this;
if (path == null) path = nil;
          return self.$xpath(path)}, TMP_5.$$s = self, TMP_5), $a).call($b).$flatten().$uniq();
        };

        def.$at = function(path) {
          var $a, self = this;

          return ((($a = self.$xpath(path).$first()) !== false && $a !== nil) ? $a : self.$css(path).$first());
        };

        def.$at_css = function(rules) {try {

          var $a, $b, TMP_6, self = this;

          rules = $slice.call(arguments, 0);
          ($a = ($b = rules).$each, $a.$$p = (TMP_6 = function(rule){var self = TMP_6.$$s || this, found = nil;
if (rule == null) rule = nil;
          found = self.$css(rule).$first();
            if (found !== false && found !== nil) {
              Opal.ret(found)
              } else {
              return nil
            };}, TMP_6.$$s = self, TMP_6), $a).call($b);
          return nil;
          } catch ($returner) { if ($returner === Opal.returner) { return $returner.$v } throw $returner; }
        };

        def.$at_xpath = function(paths) {try {

          var $a, $b, TMP_7, self = this;

          paths = $slice.call(arguments, 0);
          ($a = ($b = paths).$each, $a.$$p = (TMP_7 = function(path){var self = TMP_7.$$s || this, found = nil;
if (path == null) path = nil;
          found = self.$xpath(path).$first();
            if (found !== false && found !== nil) {
              Opal.ret(found)
              } else {
              return nil
            };}, TMP_7.$$s = self, TMP_7), $a).call($b);
          return nil;
          } catch ($returner) { if ($returner === Opal.returner) { return $returner.$v } throw $returner; }
        };

        def.$search = function(selectors) {
          var $a, $b, TMP_8, self = this;

          selectors = $slice.call(arguments, 0);
          return $scope.get('NodeSet').$new(self.$document(), ($a = ($b = selectors).$map, $a.$$p = (TMP_8 = function(selector){var self = TMP_8.$$s || this;
if (selector == null) selector = nil;
          return self.$xpath(selector).$to_a().$concat(self.$css(selector).$to_a())}, TMP_8.$$s = self, TMP_8), $a).call($b).$flatten().$uniq());
        };

        def.$css = function(path) {
          var self = this;

          return $scope.get('NodeSet').$new(self.$document(), (($scope.get('Native')).$$scope.get('Array')).$new(self["native"].querySelectorAll(path)));
        };

        def.$xpath = function(path) {
          var self = this, result = nil;

          result = [];
          try {
          
        var tmp = (self["native"].ownerDocument || self["native"]).evaluate(
          path, self["native"], null, XPathResult.ORDERED_NODE_SNAPSHOT_TYPE, null);

        result = (($scope.get('Native')).$$scope.get('Array')).$new(tmp, $hash2(["get", "length"], {"get": "snapshotItem", "length": "snapshotLength"}));
      ;
          } catch ($err) {if (true) {
            nil
            }else { throw $err; }
          };
          return $scope.get('NodeSet').$new(self.$document(), result);
        };

        def.$style = TMP_9 = function(data) {
          var $a, $b, self = this, $iter = TMP_9.$$p, block = $iter || nil, style = nil;

          if (data == null) {
            data = nil
          }
          TMP_9.$$p = null;
          style = (($scope.get('CSS')).$$scope.get('Declaration')).$new(self["native"].style);
          if ((($a = ((($b = data) !== false && $b !== nil) ? $b : block)) !== nil && (!$a.$$is_boolean || $a == true))) {
            } else {
            return style
          };
          if ((($a = data['$is_a?']($scope.get('String'))) !== nil && (!$a.$$is_boolean || $a == true))) {
            style.$replace(data)
          } else if ((($a = data['$is_a?']($scope.get('Enumerable'))) !== nil && (!$a.$$is_boolean || $a == true))) {
            style.$assign(data)};
          if (block !== false && block !== nil) {
            ($a = ($b = style).$apply, $a.$$p = block.$to_proc(), $a).call($b)};
          return self;
        };

        def['$style!'] = function() {
          var self = this;

          return (($scope.get('CSS')).$$scope.get('Declaration')).$new(self.$window().$to_n().getComputedStyle(self["native"], null));
        };

        def.$data = function(what) {
          var $a, $b, TMP_10, self = this;

          if ((($a = (typeof(self["native"].$data) !== "undefined")) !== nil && (!$a.$$is_boolean || $a == true))) {
            } else {
            self["native"].$data = {};
          };
          if ((($a = $scope.get('Hash')['$==='](what)) !== nil && (!$a.$$is_boolean || $a == true))) {
            return ($a = ($b = what).$each, $a.$$p = (TMP_10 = function(name, value){var self = TMP_10.$$s || this;
              if (self["native"] == null) self["native"] = nil;
if (name == null) name = nil;if (value == null) value = nil;
            return self["native"].$data[name] = value;}, TMP_10.$$s = self, TMP_10), $a).call($b)
            } else {
            
        var value = self["native"].$data[what];

        if (value === undefined) {
          return nil;
        }
        else {
          return value;
        }
      ;
          };
        };

        def['$matches?'] = function(selector) {
          var self = this;

          return self["native"].matches(selector);
        };

        def.$window = function() {
          var self = this;

          return self.$document().$window();
        };

        def.$inspect = function() {
          var self = this;

          return "#<DOM::Element: " + (self.$name()) + ">";
        };

        return (function($base, $super) {
          function $Attributes(){};
          var self = $Attributes = $klass($base, $super, 'Attributes', $Attributes);

          var def = self.$$proto, $scope = self.$$scope, TMP_11;

          def.element = def.namespace = nil;
          self.$include($scope.get('Enumerable'));

          self.$attr_reader("namespace");

          def.$initialize = function(element, options) {
            var self = this;

            self.element = element;
            return self.namespace = options['$[]']("namespace");
          };

          def.$each = TMP_11 = function() {
            var $a, $b, TMP_12, self = this, $iter = TMP_11.$$p, block = $iter || nil;

            TMP_11.$$p = null;
            if ((block !== nil)) {
              } else {
              return self.$enum_for("each")
            };
            ($a = ($b = self.element.$attribute_nodes()).$each, $a.$$p = (TMP_12 = function(attr){var self = TMP_12.$$s || this, $a;
if (attr == null) attr = nil;
            return $a = Opal.yieldX(block, [attr.$name(), attr.$value()]), $a === $breaker ? $a : $a}, TMP_12.$$s = self, TMP_12), $a).call($b);
            return self;
          };

          def['$[]'] = function(name) {
            var self = this;

            return self.element.$get_attribute(name, $hash2(["namespace"], {"namespace": self.namespace}));
          };

          def['$[]='] = function(name, value) {
            var self = this;

            return self.element.$set_attribute(name, value, $hash2(["namespace"], {"namespace": self.namespace}));
          };

          return (def['$merge!'] = function(hash) {
            var $a, $b, TMP_13, self = this;

            ($a = ($b = hash).$each, $a.$$p = (TMP_13 = function(name, value){var self = TMP_13.$$s || this;
if (name == null) name = nil;if (value == null) value = nil;
            return self['$[]='](name, value)}, TMP_13.$$s = self, TMP_13), $a).call($b);
            return self;
          }, nil) && 'merge!';
        })(self, null);
      })(self, $scope.get('Node'))
    })(self)
  })(self);
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/location"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass;

  Opal.add_stubs(['$include', '$to_s', '$alias_native', '$new']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base, $super) {
      function $Location(){};
      var self = $Location = $klass($base, $super, 'Location', $Location);

      var def = self.$$proto, $scope = self.$$scope;

      def["native"] = nil;
      self.$include($scope.get('Native'));

      def.$assign = function(url) {
        var self = this;

        return self["native"].assign(url.$to_s());
      };

      def.$replace = function(url) {
        var self = this;

        return self["native"].replace(url.$to_s());
      };

      def.$reload = function(force) {
        var self = this;

        if (force == null) {
          force = false
        }
        return self["native"].reload(force);
      };

      def.$to_s = function() {
        var self = this;

        return self["native"].toString();
      };

      self.$alias_native("fragment", "hash");

      self.$alias_native("fragment=", "hash=");

      self.$alias_native("host");

      self.$alias_native("host=");

      self.$alias_native("uri", "href");

      self.$alias_native("uri=", "href=");

      self.$alias_native("path", "pathname");

      self.$alias_native("path=", "pathname=");

      self.$alias_native("port");

      self.$alias_native("port=");

      self.$alias_native("scheme", "protocol");

      self.$alias_native("scheme=", "protocol=");

      self.$alias_native("query", "search");

      return self.$alias_native("query=", "search=");
    })(self, null);

    (function($base, $super) {
      function $Window(){};
      var self = $Window = $klass($base, $super, 'Window', $Window);

      var def = self.$$proto, $scope = self.$$scope;

      def["native"] = nil;
      return (def.$location = function() {
        var $a, self = this;

        if ((($a = self["native"].location) !== nil && (!$a.$$is_boolean || $a == true))) {
          return $scope.get('Location').$new(self["native"].location)
          } else {
          return nil
        };
      }, nil) && 'location'
    })(self, null);
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/document"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass, $hash2 = Opal.hash2;

  Opal.add_stubs(['$require', '$[]', '$DOM', '$new', '$first', '$xpath', '$css', '$inspect', '$children', '$convert']);
  self.$require("browser/location");
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Document(){};
        var self = $Document = $klass($base, $super, 'Document', $Document);

        var def = self.$$proto, $scope = self.$$scope;

        def["native"] = nil;
        def.$create_element = function(name, options) {
          var $a, self = this, ns = nil;

          if (options == null) {
            options = $hash2([], {})
          }
          if ((($a = ns = options['$[]']("namespace")) !== nil && (!$a.$$is_boolean || $a == true))) {
            return self.$DOM(self["native"].createElementNS(ns, name))
            } else {
            return self.$DOM(self["native"].createElement(name))
          };
        };

        def.$window = function() {
          var self = this;

          return $scope.get('Window').$new(self["native"].defaultView);
        };

        def.$create_text = function(content) {
          var self = this;

          return self.$DOM(self["native"].createTextNode(content));
        };

        def['$[]'] = function(what) {
          var $a, self = this;

          
      var result = self["native"].getElementById(what);

      if (result) {
        return self.$DOM(result);
      }
    ;
          return ((($a = self.$xpath(what).$first()) !== false && $a !== nil) ? $a : self.$css(what).$first());
        };

        Opal.defn(self, '$at', def['$[]']);

        def.$cookies = function() {
          var $a, self = this;

          if ((($a = (typeof(self["native"].cookie) !== "undefined")) !== nil && (!$a.$$is_boolean || $a == true))) {
            return $scope.get('Cookies').$new(self["native"])
            } else {
            return nil
          };
        };

        def.$document = function() {
          var self = this;

          return self;
        };

        def.$inspect = function() {
          var self = this;

          return "#<DOM::Document: " + (self.$children().$inspect()) + ">";
        };

        def.$location = function() {
          var $a, self = this;

          if ((($a = self["native"].location) !== nil && (!$a.$$is_boolean || $a == true))) {
            return $scope.get('Location').$new(self["native"].location)
            } else {
            return nil
          };
        };

        def.$title = function() {
          var self = this;

          return self["native"].title;
        };

        def['$title='] = function(value) {
          var self = this;

          return self["native"].title = value;
        };

        def.$root = function() {
          var self = this;

          return self.$DOM(self["native"].documentElement);
        };

        def.$head = function() {
          var self = this;

          return self.$DOM(self["native"].getElementsByTagName("head")[0]);
        };

        def.$body = function() {
          var self = this;

          return self.$DOM(self["native"].body);
        };

        def.$style_sheets = function() {
          var $a, $b, TMP_1, self = this;

          return ($a = ($b = (($scope.get('Native')).$$scope.get('Array'))).$new, $a.$$p = (TMP_1 = function(e){var self = TMP_1.$$s || this;
if (e == null) e = nil;
          return (($scope.get('CSS')).$$scope.get('StyleSheet')).$new(e)}, TMP_1.$$s = self, TMP_1), $a).call($b, self["native"].styleSheets);
        };

        return (def['$root='] = function(element) {
          var self = this;

          return self["native"].documentElement = $scope.get('Native').$convert(element);
        }, nil) && 'root=';
      })(self, $scope.get('Element'))
    })(self)
  })(self);
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/document_fragment"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass;

  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $DocumentFragment(){};
        var self = $DocumentFragment = $klass($base, $super, 'DocumentFragment', $DocumentFragment);

        var def = self.$$proto, $scope = self.$$scope;

        return nil;
      })(self, $scope.get('Element'))
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/builder"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass, $hash2 = Opal.hash2;

  Opal.add_stubs(['$<<', '$[]=', '$to_h', '$[]', '$each', '$===', '$call', '$attr_reader', '$new', '$to_proc', '$map', '$build', '$for', '$create_text', '$document', '$create_element', '$merge!', '$attributes', '$add_class', '$on', '$style', '$inner_html=']);
  (function($base) {
    var self = $module($base, 'Utils');

    var def = self.$$proto, $scope = self.$$scope;

    Opal.defs(self, '$heredoc', function(string) {
      var self = this;

      return string;
    })
  })($scope.get('Paggio'));
  (function($base, $super) {
    function $Element(){};
    var self = $Element = $klass($base, $super, 'Element', $Element);

    var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_2;

    def.on = nil;
    def.$on = TMP_1 = function(args) {
      var $a, self = this, $iter = TMP_1.$$p, block = $iter || nil;

      args = $slice.call(arguments, 0);
      TMP_1.$$p = null;
      return (((($a = self.on) !== false && $a !== nil) ? $a : self.on = []))['$<<']([args, block]);
    };

    return (def.$style = TMP_2 = function(args) {
      var self = this, $iter = TMP_2.$$p, block = $iter || nil;

      args = $slice.call(arguments, 0);
      TMP_2.$$p = null;
      return self.style = [args, block];
    }, nil) && 'style';
  })((($scope.get('Paggio')).$$scope.get('HTML')), $scope.get('BasicObject'));
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope, $a, $b, TMP_8, $c, TMP_9;

      (function($base, $super) {
        function $Builder(){};
        var self = $Builder = $klass($base, $super, 'Builder', $Builder);

        var def = self.$$proto, $scope = self.$$scope, TMP_3, TMP_5;

        def.builder = def.element = def.roots = nil;
        Opal.defs(self, '$to_h', function() {
          var $a, self = this;
          if (self.builders == null) self.builders = nil;

          return ((($a = self.builders) !== false && $a !== nil) ? $a : self.builders = $hash2([], {}));
        });

        Opal.defs(self, '$for', TMP_3 = function(klass) {
          var self = this, $iter = TMP_3.$$p, block = $iter || nil;

          TMP_3.$$p = null;
          if (block !== false && block !== nil) {
            return self.$to_h()['$[]='](klass, block)
            } else {
            return self.$to_h()['$[]'](klass)
          };
        });

        Opal.defs(self, '$build', function(builder, item) {
          var $a, $b, TMP_4, self = this;

          return ($a = ($b = self.$to_h()).$each, $a.$$p = (TMP_4 = function(klass, block){var self = TMP_4.$$s || this, $a;
if (klass == null) klass = nil;if (block == null) block = nil;
          if ((($a = klass['$==='](item)) !== nil && (!$a.$$is_boolean || $a == true))) {
              return ($breaker.$v = block.$call(builder, item), $breaker)
              } else {
              return nil
            }}, TMP_4.$$s = self, TMP_4), $a).call($b);
        });

        self.$attr_reader("document", "element");

        def.$initialize = TMP_5 = function(document, element) {
          var $a, $b, $c, TMP_6, $d, TMP_7, self = this, $iter = TMP_5.$$p, block = $iter || nil;

          if (element == null) {
            element = nil
          }
          TMP_5.$$p = null;
          self.document = document;
          self.element = element;
          self.builder = ($a = ($b = (($scope.get('Paggio')).$$scope.get('HTML'))).$new, $a.$$p = block.$to_proc(), $a).call($b);
          self.roots = ($a = ($c = self.builder.$each()).$map, $a.$$p = (TMP_6 = function(e){var self = TMP_6.$$s || this;
if (e == null) e = nil;
          return $scope.get('Builder').$build(self, e)}, TMP_6.$$s = self, TMP_6), $a).call($c);
          if ((($a = self.element) !== nil && (!$a.$$is_boolean || $a == true))) {
            return ($a = ($d = self.roots).$each, $a.$$p = (TMP_7 = function(root){var self = TMP_7.$$s || this;
              if (self.element == null) self.element = nil;
if (root == null) root = nil;
            return self.element['$<<'](root)}, TMP_7.$$s = self, TMP_7), $a).call($d)
            } else {
            return nil
          };
        };

        return (def.$to_a = function() {
          var self = this;

          return self.roots;
        }, nil) && 'to_a';
      })(self, null);

      ($a = ($b = $scope.get('Builder')).$for, $a.$$p = (TMP_8 = function(b, item){var self = TMP_8.$$s || this;
if (b == null) b = nil;if (item == null) item = nil;
      return b.$document().$create_text(item)}, TMP_8.$$s = self, TMP_8), $a).call($b, $scope.get('String'));

      ($a = ($c = $scope.get('Builder')).$for, $a.$$p = (TMP_9 = function(b, item){var self = TMP_9.$$s || this, $a, $b, TMP_10, $c, TMP_11, $d, $e, TMP_12, dom = nil, on = nil, style = nil, inner = nil;
if (b == null) b = nil;if (item == null) item = nil;
      dom = b.$document().$create_element(item.name);
        if ((($a = $scope.get('Hash')['$==='](item.attributes)) !== nil && (!$a.$$is_boolean || $a == true))) {
          dom.$attributes()['$merge!'](item.attributes)};
        ($a = ($b = (item.class_names)).$each, $a.$$p = (TMP_10 = function(value){var self = TMP_10.$$s || this;
if (value == null) value = nil;
        return dom.$add_class(value)}, TMP_10.$$s = self, TMP_10), $a).call($b);
        if ((($a = on = item.on || nil) !== nil && (!$a.$$is_boolean || $a == true))) {
          ($a = ($c = on).$each, $a.$$p = (TMP_11 = function(args, block){var self = TMP_11.$$s || this, $a, $b;
if (args == null) args = nil;if (block == null) block = nil;
          return ($a = ($b = dom).$on, $a.$$p = block.$to_proc(), $a).apply($b, [].concat(args))}, TMP_11.$$s = self, TMP_11), $a).call($c)};
        if ((($a = style = item.style || nil) !== nil && (!$a.$$is_boolean || $a == true))) {
          ($a = ($d = dom).$style, $a.$$p = style['$[]'](1).$to_proc(), $a).apply($d, [].concat(style['$[]'](0)))};
        if ((($a = inner = item.inner_html || nil) !== nil && (!$a.$$is_boolean || $a == true))) {
          (($a = [inner]), $e = dom, $e['$inner_html='].apply($e, $a), $a[$a.length-1])
          } else {
          ($a = ($e = item).$each, $a.$$p = (TMP_12 = function(child){var self = TMP_12.$$s || this;
if (child == null) child = nil;
          return dom['$<<']($scope.get('Builder').$build(b, child))}, TMP_12.$$s = self, TMP_12), $a).call($e)
        };
        return dom;}, TMP_9.$$s = self, TMP_9), $a).call($c, (((($scope.get('Paggio')).$$scope.get('HTML'))).$$scope.get('Element')));
    })(self)
  })(self);
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/mutation_observer"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass, $gvars = Opal.gvars, $hash2 = Opal.hash2;

  Opal.add_stubs(['$include', '$===', '$==', '$type', '$new', '$DOM', '$alias_native', '$call', '$map', '$convert', '$private', '$Native', '$[]', '$[]=', '$to_n']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $MutationObserver(){};
        var self = $MutationObserver = $klass($base, $super, 'MutationObserver', $MutationObserver);

        var def = self.$$proto, $scope = self.$$scope, TMP_1;

        def["native"] = nil;
        self.$include($scope.get('Native'));

        (function($base, $super) {
          function $Record(){};
          var self = $Record = $klass($base, $super, 'Record', $Record);

          var def = self.$$proto, $scope = self.$$scope;

          def["native"] = nil;
          self.$include($scope.get('Native'));

          def.$type = function() {
            var self = this, $case = nil;

            return (function() {$case = self["native"].type;if ("attributes"['$===']($case)) {return "attributes"}else if ("childList"['$===']($case)) {return "tree"}else if ("characterData"['$===']($case)) {return "cdata"}else { return nil }})();
          };

          def['$attributes?'] = function() {
            var self = this;

            return self.$type()['$==']("attributes");
          };

          def['$tree?'] = function() {
            var self = this;

            return self.$type()['$==']("tree");
          };

          def['$cdata?'] = function() {
            var self = this;

            return self.$type()['$==']("cdata");
          };

          def.$added = function() {
            var $a, self = this, array = nil;
            if ($gvars.document == null) $gvars.document = nil;

            array = (function() {if ((($a = self["native"].addedNodes != null) !== nil && (!$a.$$is_boolean || $a == true))) {
              return (($scope.get('Native')).$$scope.get('Array')).$new(self["native"].addedNodes)
              } else {
              return []
            }; return nil; })();
            return $scope.get('NodeSet').$new($gvars.document, array);
          };

          def.$removed = function() {
            var $a, self = this, array = nil;
            if ($gvars.document == null) $gvars.document = nil;

            array = (function() {if ((($a = self["native"].removedNodes != null) !== nil && (!$a.$$is_boolean || $a == true))) {
              return (($scope.get('Native')).$$scope.get('Array')).$new(self["native"].removedNodes)
              } else {
              return []
            }; return nil; })();
            return $scope.get('NodeSet').$new($gvars.document, array);
          };

          def.$target = function() {
            var self = this;

            return self.$DOM(self["native"].target);
          };

          self.$alias_native("old", "oldValue");

          return self.$alias_native("attribute", "attributeName");
        })(self, null);

        def.$initialize = TMP_1 = function() {
          var $a, $b, TMP_2, self = this, $iter = TMP_1.$$p, block = $iter || nil;

          TMP_1.$$p = null;
          
      var func = function(records) {
        return block.$call(($a = ($b = (records)).$map, $a.$$p = (TMP_2 = function(r){var self = TMP_2.$$s || this;
if (r == null) r = nil;
          return (((((($scope.get('Browser')).$$scope.get('DOM'))).$$scope.get('MutationObserver'))).$$scope.get('Record')).$new(r)}, TMP_2.$$s = self, TMP_2), $a).call($b));
      }
    ;
          return Opal.find_super_dispatcher(self, 'initialize', TMP_1, null).apply(self, [new window.MutationObserver(func)]);
        };

        def.$observe = function(target, options) {
          var self = this;

          if (options == null) {
            options = nil
          }
          if (options !== false && options !== nil) {
            } else {
            options = $hash2(["children", "tree", "attributes", "cdata"], {"children": true, "tree": true, "attributes": "old", "cdata": "old"})
          };
          self["native"].observe($scope.get('Native').$convert(target), self.$convert(options));
          return self;
        };

        def.$take = function() {
          var $a, $b, TMP_3, self = this;

          return ($a = ($b = (self["native"].takeRecords())).$map, $a.$$p = (TMP_3 = function(r){var self = TMP_3.$$s || this;
if (r == null) r = nil;
          return $scope.get('Record').$new(r)}, TMP_3.$$s = self, TMP_3), $a).call($b);
        };

        def.$disconnect = function() {
          var self = this;

          return self["native"].disconnect();
        };

        self.$private();

        return (def.$convert = function(hash) {
          var $a, self = this, options = nil, attrs = nil, filter = nil, cdata = nil;

          options = self.$Native({});
          if ((($a = hash['$[]']("children")) !== nil && (!$a.$$is_boolean || $a == true))) {
            options['$[]=']("childList", true)};
          if ((($a = hash['$[]']("tree")) !== nil && (!$a.$$is_boolean || $a == true))) {
            options['$[]=']("subtree", true)};
          if ((($a = attrs = hash['$[]']("attributes")) !== nil && (!$a.$$is_boolean || $a == true))) {
            options['$[]=']("attributes", true);
            if (attrs['$==']("old")) {
              options['$[]=']("attributeOldValue", true)};};
          if ((($a = filter = hash['$[]']("filter")) !== nil && (!$a.$$is_boolean || $a == true))) {
            options['$[]=']("attributeFilter", filter)};
          if ((($a = cdata = hash['$[]']("cdata")) !== nil && (!$a.$$is_boolean || $a == true))) {
            options['$[]=']("characterData", true);
            if (cdata['$==']("old")) {
              options['$[]=']("characterDataOldValue", true)};};
          return options.$to_n();
        }, nil) && 'convert';
      })(self, null)
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/compatibility/dom/document/window"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass;

  Opal.add_stubs(['$has?', '$raise']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Document(){};
        var self = $Document = $klass($base, $super, 'Document', $Document);

        var def = self.$$proto, $scope = self.$$scope, $a;

        def["native"] = nil;
        if ((($a = $scope.get('C')['$has?'](document, "defaultView")) !== nil && (!$a.$$is_boolean || $a == true))) {
          return nil
        } else if ((($a = $scope.get('C')['$has?'](document, "parentWindow")) !== nil && (!$a.$$is_boolean || $a == true))) {
          return (def.$window = function() {
            var self = this;

            return self["native"].parentWindow;
          }, nil) && 'window'
          } else {
          return (def.$window = function() {
            var self = this;

            return self.$raise($scope.get('NotImplementedError'), "window from document is unsupported");
          }, nil) && 'window'
        }
      })(self, $scope.get('Element'))
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/compatibility/dom/mutation_observer"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass, $hash2 = Opal.hash2;

  Opal.add_stubs(['$attr_reader', '$==', '$type']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope, $a;

      if ((($a = (typeof(window.MutationObserver) !== "undefined")) !== nil && (!$a.$$is_boolean || $a == true))) {
        } else {
        (function($base, $super) {
          function $MutationObserver(){};
          var self = $MutationObserver = $klass($base, $super, 'MutationObserver', $MutationObserver);

          var def = self.$$proto, $scope = self.$$scope, TMP_1;

          def.records = nil;
          (function($base, $super) {
            function $Record(){};
            var self = $Record = $klass($base, $super, 'Record', $Record);

            var def = self.$$proto, $scope = self.$$scope;

            self.$attr_reader("type", "target", "old", "attribute");

            def.$initialize = function() {
              var self = this;

              return nil;
            };

            def['$attributes?'] = function() {
              var self = this;

              return self.$type()['$==']("attributes");
            };

            def['$tree?'] = function() {
              var self = this;

              return self.$type()['$==']("tree");
            };

            return (def['$cdata?'] = function() {
              var self = this;

              return self.$type()['$==']("cdata");
            }, nil) && 'cdata?';
          })(self, null);

          def.$initialize = TMP_1 = function() {
            var self = this, $iter = TMP_1.$$p, block = $iter || nil;

            TMP_1.$$p = null;
            self.block = block;
            return self.observed = [];
          };

          def.$observe = function(target, options) {
            var self = this;

            if (options == null) {
              options = nil
            }
            if (options !== false && options !== nil) {
              } else {
              options = $hash2(["children", "tree", "attributes", "cdata"], {"children": true, "tree": true, "attributes": "old", "cdata": "old"})
            };
            return self;
          };

          def.$take = function() {
            var $a, self = this, result = nil;

            $a = [self.records, []], result = $a[0], self.records = $a[1];
            return result;
          };

          return (def.$disconnect = function() {
            var self = this;

            return nil;
          }, nil) && 'disconnect';
        })(self, null)
      }
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/compatibility/dom/element/matches"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass;

  Opal.add_stubs(['$respond_to?', '$sizzle?', '$raise']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Element(){};
        var self = $Element = $klass($base, $super, 'Element', $Element);

        var def = self.$$proto, $scope = self.$$scope, $a;

        def["native"] = nil;
        if ((($a = $scope.get('C')['$respond_to?']("Element", "matches")) !== nil && (!$a.$$is_boolean || $a == true))) {
          return nil
        } else if ((($a = $scope.get('C')['$respond_to?']("Element", "oMatchesSelector")) !== nil && (!$a.$$is_boolean || $a == true))) {
          return (def['$matches?'] = function(selector) {
            var self = this;

            return self["native"].oMatchesSelector(selector);
          }, nil) && 'matches?'
        } else if ((($a = $scope.get('C')['$respond_to?']("Element", "msMatchesSelector")) !== nil && (!$a.$$is_boolean || $a == true))) {
          return (def['$matches?'] = function(selector) {
            var self = this;

            return self["native"].msMatchesSelector(selector);
          }, nil) && 'matches?'
        } else if ((($a = $scope.get('C')['$respond_to?']("Element", "mozMatchesSelector")) !== nil && (!$a.$$is_boolean || $a == true))) {
          return (def['$matches?'] = function(selector) {
            var self = this;

            return self["native"].mozMatchesSelector(selector);
          }, nil) && 'matches?'
        } else if ((($a = $scope.get('C')['$respond_to?']("Element", "webkitMatchesSelector")) !== nil && (!$a.$$is_boolean || $a == true))) {
          return (def['$matches?'] = function(selector) {
            var self = this;

            return self["native"].webkitMatchesSelector(selector);
          }, nil) && 'matches?'
        } else if ((($a = $scope.get('C')['$sizzle?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
          return (def['$matches?'] = function(selector) {
            var self = this;

            return Sizzle.matchesSelector(self["native"], selector);
          }, nil) && 'matches?'
          } else {
          return (def['$matches?'] = function() {
            var self = this;

            return self.$raise($scope.get('NotImplementedError'), "matches by selector unsupported");
          }, nil) && 'matches?'
        }
      })(self, null)
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/compatibility/dom/element/css"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass;

  Opal.add_stubs(['$respond_to?', '$sizzle?', '$new', '$document', '$raise']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Element(){};
        var self = $Element = $klass($base, $super, 'Element', $Element);

        var def = self.$$proto, $scope = self.$$scope, $a;

        def["native"] = nil;
        if ((($a = $scope.get('C')['$respond_to?']("Element", "querySelectorAll")) !== nil && (!$a.$$is_boolean || $a == true))) {
          return nil
        } else if ((($a = $scope.get('C')['$sizzle?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
          return (def.$css = function(path) {
            var self = this;

            return $scope.get('NodeSet').$new(self.$document(), Sizzle(path, self["native"]));
          }, nil) && 'css'
          } else {
          return (def.$css = function() {
            var self = this;

            return self.$raise($scope.get('NotImplementedError'), "fetching by selector unsupported");
          }, nil) && 'css'
        }
      })(self, null)
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/compatibility/dom/element/offset"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass;

  Opal.add_stubs(['$has?', '$document', '$to_n', '$root', '$window', '$new']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Element(){};
        var self = $Element = $klass($base, $super, 'Element', $Element);

        var def = self.$$proto, $scope = self.$$scope;

        return (function($base, $super) {
          function $Offset(){};
          var self = $Offset = $klass($base, $super, 'Offset', $Offset);

          var def = self.$$proto, $scope = self.$$scope, $a;

          if ((($a = $scope.get('C')['$has?'](document.body, "getBoundingClientRect")) !== nil && (!$a.$$is_boolean || $a == true))) {
            return nil
            } else {
            return (def.$position = function() {
              var self = this, doc = nil, root = nil, win = nil;

              doc = self.$document();
              root = doc.$root().$to_n();
              win = doc.$window().$to_n();
              
        var y = (win.pageYOffset || root.scrollTop) - (root.clientTop || 0),
            x = (win.pageXOffset || root.scrollLeft) - (root.clientLeft || 0);
      ;
              return (($scope.get('Browser')).$$scope.get('Position')).$new(x, y);
            }, nil) && 'position'
          }
        })(self, null)
      })(self, null)
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/compatibility/dom/element/style"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass;

  Opal.add_stubs(['$has?', '$new', '$raise']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'DOM');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Element(){};
        var self = $Element = $klass($base, $super, 'Element', $Element);

        var def = self.$$proto, $scope = self.$$scope, $a;

        def["native"] = nil;
        if ((($a = $scope.get('C')['$has?']("getComputedStyle")) !== nil && (!$a.$$is_boolean || $a == true))) {
          return nil
        } else if ((($a = $scope.get('C')['$has?'](document.documentElement, "currentStyle")) !== nil && (!$a.$$is_boolean || $a == true))) {
          return (def['$style!'] = function() {
            var self = this;

            return (($scope.get('CSS')).$$scope.get('Declaration')).$new(self["native"].currentStyle);
          }, nil) && 'style!'
          } else {
          return (def['$style!'] = function() {
            var self = this;

            return self.$raise($scope.get('NotImplementedError'), "computed style unsupported");
          }, nil) && 'style!'
        }
      })(self, null)
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom/compatibility"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice;

  Opal.add_stubs(['$require']);
  self.$require("browser/compatibility/dom/document/window");
  self.$require("browser/compatibility/dom/mutation_observer");
  self.$require("browser/compatibility/dom/element/matches");
  self.$require("browser/compatibility/dom/element/css");
  self.$require("browser/compatibility/dom/element/offset");
  return self.$require("browser/compatibility/dom/element/style");
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/dom"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $gvars = Opal.gvars, $klass = Opal.klass;
  if ($gvars.window == null) $gvars.window = nil;

  Opal.add_stubs(['$require', '$DOM', '$shift', '$to_a', '$new', '$to_proc', '$==', '$length', '$first', '$native?', '$===', '$try_convert', '$raise', '$include', '$target', '$document']);
  self.$require("browser/dom/event");
  self.$require("browser/dom/node_set");
  self.$require("browser/dom/node");
  self.$require("browser/dom/attribute");
  self.$require("browser/dom/character_data");
  self.$require("browser/dom/text");
  self.$require("browser/dom/cdata");
  self.$require("browser/dom/comment");
  self.$require("browser/dom/element");
  self.$require("browser/dom/document");
  self.$require("browser/dom/document_fragment");
  self.$require("browser/dom/builder");
  self.$require("browser/dom/mutation_observer");
  self.$require("browser/dom/compatibility");
  (function($base) {
    var self = $module($base, 'Kernel');

    var def = self.$$proto, $scope = self.$$scope, TMP_1;

    Opal.defn(self, '$XML', function(what) {
      var self = this;

      
      var doc;

      if (window.DOMParser) {
        doc = new DOMParser().parseFromString(what, 'text/xml');
      }
      else {
        doc       = new ActiveXObject('Microsoft.XMLDOM');
        doc.async = 'false';
        doc.loadXML(what);
      }
    
      return self.$DOM(doc);
    });

    Opal.defn(self, '$DOM', TMP_1 = function(args) {
      var $a, $b, self = this, $iter = TMP_1.$$p, block = $iter || nil, document = nil, element = nil, roots = nil, what = nil;
      if ($gvars.document == null) $gvars.document = nil;

      args = $slice.call(arguments, 0);
      TMP_1.$$p = null;
      if (block !== false && block !== nil) {
        document = ((($a = args.$shift()) !== false && $a !== nil) ? $a : $gvars.document);
        element = args.$shift();
        roots = ($a = ($b = (((($scope.get('Browser')).$$scope.get('DOM'))).$$scope.get('Builder'))).$new, $a.$$p = block.$to_proc(), $a).call($b, document, element).$to_a();
        if (roots.$length()['$=='](1)) {
          return roots.$first()
          } else {
          return (((($scope.get('Browser')).$$scope.get('DOM'))).$$scope.get('NodeSet')).$new(document, roots)
        };
        } else {
        what = args.$shift();
        document = ((($a = args.$shift()) !== false && $a !== nil) ? $a : $gvars.document);
        if ((($a = self['$native?'](what)) !== nil && (!$a.$$is_boolean || $a == true))) {
          return (((($scope.get('Browser')).$$scope.get('DOM'))).$$scope.get('Node')).$new(what)
        } else if ((($a = (((($scope.get('Browser')).$$scope.get('DOM'))).$$scope.get('Node'))['$==='](what)) !== nil && (!$a.$$is_boolean || $a == true))) {
          return what
        } else if ((($a = $scope.get('String')['$==='](what)) !== nil && (!$a.$$is_boolean || $a == true))) {
          
          var doc = $scope.get('Native').$try_convert(document).createElement('div');
          doc.innerHTML = what;

          return self.$DOM(doc.childNodes.length == 1 ? doc.childNodes[0] : doc);
        ;
          } else {
          return self.$raise($scope.get('ArgumentError'), "argument not DOM convertible")
        };
      };
    });
  })(self);
  (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base, $super) {
      function $Window(){};
      var self = $Window = $klass($base, $super, 'Window', $Window);

      var def = self.$$proto, $scope = self.$$scope, $a, $b, TMP_2;

      def["native"] = nil;
      self.$include((((($scope.get('DOM')).$$scope.get('Event'))).$$scope.get('Target')));

      ($a = ($b = self).$target, $a.$$p = (TMP_2 = function(value){var self = TMP_2.$$s || this, $a;
        if ($gvars.window == null) $gvars.window = nil;
if (value == null) value = nil;
      if ((($a = value == window) !== nil && (!$a.$$is_boolean || $a == true))) {
          return $gvars.window
          } else {
          return nil
        }}, TMP_2.$$s = self, TMP_2), $a).call($b);

      return (def.$document = function() {
        var self = this;

        return self.$DOM(self["native"].document);
      }, nil) && 'document';
    })(self, null)
  })(self);
  return $gvars.document = $gvars.window.$document();
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/css/declaration"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass, $range = Opal.range;

  Opal.add_stubs(['$include', '$new', '$each', '$[]=', '$important', '$name', '$value', '$to_proc', '$to_s', '$enum_for', '$[]', '$alias_native', '$end_with?']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'CSS');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Declaration(){};
        var self = $Declaration = $klass($base, $super, 'Declaration', $Declaration);

        var def = self.$$proto, $scope = self.$$scope, TMP_2, TMP_4;

        def["native"] = nil;
        self.$include($scope.get('Native'));

        self.$include($scope.get('Enumerable'));

        def.$rule = function() {
          var $a, self = this;

          if ((($a = (typeof(self["native"].parentRule) !== "undefined")) !== nil && (!$a.$$is_boolean || $a == true))) {
            return $scope.get('Rule').$new(self["native"].parentRule)
            } else {
            return nil
          };
        };

        def.$assign = function(data) {
          var $a, $b, TMP_1, self = this;

          ($a = ($b = data).$each, $a.$$p = (TMP_1 = function(name, value){var self = TMP_1.$$s || this;
if (name == null) name = nil;if (value == null) value = nil;
          return self['$[]='](name, value)}, TMP_1.$$s = self, TMP_1), $a).call($b);
          return self;
        };

        def.$replace = function(string) {
          var self = this;

          self["native"].cssText = string;
          return self;
        };

        def.$apply = TMP_2 = function() {
          var $a, $b, TMP_3, $c, $d, self = this, $iter = TMP_2.$$p, block = $iter || nil;

          TMP_2.$$p = null;
          ($a = ($b = ($c = ($d = (((($scope.get('Paggio')).$$scope.get('CSS'))).$$scope.get('Definition'))).$new, $c.$$p = block.$to_proc(), $c).call($d)).$each, $a.$$p = (TMP_3 = function(style){var self = TMP_3.$$s || this, $a;
            if (self["native"] == null) self["native"] = nil;
if (style == null) style = nil;
          if ((($a = style.$important()) !== nil && (!$a.$$is_boolean || $a == true))) {
              return self["native"].setProperty(style.$name(), style.$value(), "important");
              } else {
              return self["native"].setProperty(style.$name(), style.$value(), "");
            }}, TMP_3.$$s = self, TMP_3), $a).call($b);
          return self;
        };

        def.$delete = function(name) {
          var self = this;

          return self["native"].removeProperty(name);
        };

        def['$[]'] = function(name) {
          var self = this;

          
      var result = self["native"].getPropertyValue(name);

      if (result == null || result === "") {
        return nil;
      }

      return result;
    ;
        };

        def['$[]='] = function(name, value) {
          var self = this;

          return self["native"].setProperty(name, value.$to_s(), "");
        };

        def['$important?'] = function(name) {
          var self = this;

          return self["native"].getPropertyPriority(name) == "important";
        };

        def.$each = TMP_4 = function() {
          var $a, self = this, $iter = TMP_4.$$p, block = $iter || nil;

          TMP_4.$$p = null;
          if ((block !== nil)) {
            } else {
            return self.$enum_for("each")
          };
          
      for (var i = 0, length = self["native"].length; i < length; i++) {
        var name  = self["native"].item(i);

        ((($a = Opal.yieldX(block, [name, self['$[]'](name)])) === $breaker) ? $breaker.$v : $a)
      }
    ;
          return self;
        };

        self.$alias_native("length");

        self.$alias_native("to_s", "cssText");

        return (def.$method_missing = function(name, value) {
          var $a, self = this;

          if (value == null) {
            value = nil
          }
          if ((($a = name['$end_with?']("=")) !== nil && (!$a.$$is_boolean || $a == true))) {
            return self['$[]='](name['$[]']($range(0, -2, false)), value)
            } else {
            return self['$[]'](name)
          };
        }, nil) && 'method_missing';
      })(self, null)
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/css/style_sheet"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass;

  Opal.add_stubs(['$include', '$is_a?', '$to_n', '$alias_native', '$new', '$DOM', '$===', '$join', '$map', '$insert', '$length', '$find', '$log', '$==', '$id', '$rules', '$__send__', '$to_proc']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'CSS');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $StyleSheet(){};
        var self = $StyleSheet = $klass($base, $super, 'StyleSheet', $StyleSheet);

        var def = self.$$proto, $scope = self.$$scope, TMP_1, TMP_5;

        def["native"] = nil;
        self.$include($scope.get('Native'));

        def.$initialize = TMP_1 = function(what) {
          var $a, self = this, $iter = TMP_1.$$p, $yield = $iter || nil;

          TMP_1.$$p = null;
          if ((($a = what['$is_a?']((($scope.get('DOM')).$$scope.get('Element')))) !== nil && (!$a.$$is_boolean || $a == true))) {
            return Opal.find_super_dispatcher(self, 'initialize', TMP_1, null).apply(self, [what.$to_n().sheet])
            } else {
            return Opal.find_super_dispatcher(self, 'initialize', TMP_1, null).apply(self, [what])
          };
        };

        self.$alias_native("disabled?", "disabled");

        self.$alias_native("href");

        self.$alias_native("title");

        self.$alias_native("type");

        def.$media = function() {
          var $a, self = this;

          if ((($a = self["native"].media != null) !== nil && (!$a.$$is_boolean || $a == true))) {
            return $scope.get('Media').$new(self["native"].media)
            } else {
            return nil
          };
        };

        def.$owner = function() {
          var self = this;

          return self.$DOM(self["native"].ownerNode);
        };

        def.$parent = function() {
          var $a, self = this;

          if ((($a = self["native"].parentStyleSheet != null) !== nil && (!$a.$$is_boolean || $a == true))) {
            return $scope.get('Sheet').$new(self["native"].parentStyleSheet)
            } else {
            return nil
          };
        };

        def.$rules = function() {
          var $a, $b, TMP_2, self = this;

          return ($a = ($b = (($scope.get('Native')).$$scope.get('Array'))).$new, $a.$$p = (TMP_2 = function(e){var self = TMP_2.$$s || this;
if (e == null) e = nil;
          return $scope.get('Rule').$new(e)}, TMP_2.$$s = self, TMP_2), $a).call($b, self["native"].cssRules);
        };

        def.$delete = function(index) {
          var self = this;

          return self["native"].deleteRule(index);
        };

        def.$insert = function(index, rule) {
          var self = this;

          return self["native"].insertRule(rule, index);
        };

        def.$rule = function(selector, body) {
          var $a, $b, TMP_3, self = this;

          if ((($a = $scope.get('String')['$==='](selector)) !== nil && (!$a.$$is_boolean || $a == true))) {
            } else {
            selector = selector.$join(", ")
          };
          if ((($a = $scope.get('String')['$==='](body)) !== nil && (!$a.$$is_boolean || $a == true))) {
            } else {
            body = ($a = ($b = body).$map, $a.$$p = (TMP_3 = function(name, value){var self = TMP_3.$$s || this;
if (name == null) name = nil;if (value == null) value = nil;
            return "" + (name) + ": " + (value) + ";"}, TMP_3.$$s = self, TMP_3), $a).call($b).$join("\n")
          };
          return self.$insert(self.$length(), "" + (selector) + " { " + (body) + " }");
        };

        def['$[]'] = function(id) {
          var $a, $b, TMP_4, self = this;

          return ($a = ($b = self.$rules()).$find, $a.$$p = (TMP_4 = function(r){var self = TMP_4.$$s || this;
if (r == null) r = nil;
          self.$log(r);
            return r.$id()['$=='](id);}, TMP_4.$$s = self, TMP_4), $a).call($b);
        };

        def.$method_missing = TMP_5 = function(args) {
          var $a, $b, self = this, $iter = TMP_5.$$p, block = $iter || nil;

          args = $slice.call(arguments, 0);
          TMP_5.$$p = null;
          return ($a = ($b = self.$rules()).$__send__, $a.$$p = block.$to_proc(), $a).apply($b, [].concat(args));
        };

        return (function($base, $super) {
          function $Media(){};
          var self = $Media = $klass($base, $super, 'Media', $Media);

          var def = self.$$proto, $scope = self.$$scope;

          def["native"] = nil;
          self.$alias_native("text", "mediaText");

          self.$alias_native("to_s", "mediaText");

          def.$push = function(medium) {
            var self = this;

            self["native"].appendMedium(medium);
            return self;
          };

          return (def.$delete = function(medium) {
            var self = this;

            return self["native"].deleteMedium(medium);
          }, nil) && 'delete';
        })(self, (($scope.get('Native')).$$scope.get('Array')));
      })(self, null)
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/css/rule"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass;

  Opal.add_stubs(['$include', '$==', '$[]', '$new', '$raise', '$alias_native']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'CSS');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Rule(){};
        var self = $Rule = $klass($base, $super, 'Rule', $Rule);

        var def = self.$$proto, $scope = self.$$scope, TMP_1;

        def["native"] = nil;
        self.$include($scope.get('Native'));

        Opal.cdecl($scope, 'STYLE_RULE', 1);

        Opal.cdecl($scope, 'CHARSET_RULE', 2);

        Opal.cdecl($scope, 'IMPORT_RULE', 3);

        Opal.cdecl($scope, 'MEDIA_RULE', 4);

        Opal.cdecl($scope, 'FONT_FACE_RULE', 5);

        Opal.cdecl($scope, 'PAGE_RULE', 6);

        Opal.cdecl($scope, 'KEYFRAMES_RULE', 7);

        Opal.cdecl($scope, 'KEYFRAME_RULE', 8);

        Opal.cdecl($scope, 'NAMESPACE_RULE', 10);

        Opal.cdecl($scope, 'COUNTER_STYLE_RULE', 11);

        Opal.cdecl($scope, 'SUPPORTS_RULE', 12);

        Opal.cdecl($scope, 'DOCUMENT_RULE', 13);

        Opal.cdecl($scope, 'FONT_FEATURE_VALUES_RULE', 14);

        Opal.cdecl($scope, 'VIEWPORT_RULE', 15);

        Opal.cdecl($scope, 'REGION_STYLE_RULE', 16);

        Opal.defs(self, '$new', TMP_1 = function(rule) {
          var $a, self = this, $iter = TMP_1.$$p, $yield = $iter || nil, klass = nil;
          if (self.classes == null) self.classes = nil;

          TMP_1.$$p = null;
          if (self['$==']($scope.get('Rule'))) {
            ((($a = self.classes) !== false && $a !== nil) ? $a : self.classes = [nil, $scope.get('Style')]);
            if ((($a = klass = self.classes['$[]'](rule.type)) !== nil && (!$a.$$is_boolean || $a == true))) {
              return klass.$new(rule)
              } else {
              return self.$raise($scope.get('ArgumentError'), "cannot instantiate a non derived Rule object")
            };
            } else {
            return Opal.find_super_dispatcher(self, 'new', TMP_1, null, $Rule).apply(self, [rule])
          };
        });

        self.$alias_native("text", "cssText");

        self.$alias_native("to_s", "cssText");

        def.$parent = function() {
          var $a, self = this;

          if ((($a = self["native"].parentRule != null) !== nil && (!$a.$$is_boolean || $a == true))) {
            return $scope.get('Rule').$new(self["native"].parentRule)
            } else {
            return nil
          };
        };

        return (def.$style_sheet = function() {
          var $a, self = this;

          if ((($a = self["native"].parentStyleSheet != null) !== nil && (!$a.$$is_boolean || $a == true))) {
            return $scope.get('StyleSheet').$new(self["native"].parentStyleSheet)
            } else {
            return nil
          };
        }, nil) && 'style_sheet';
      })(self, null)
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/css/rule/style"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $klass = Opal.klass;

  Opal.add_stubs(['$alias_native', '$new', '$__send__', '$to_proc', '$declaration']);
  return (function($base) {
    var self = $module($base, 'Browser');

    var def = self.$$proto, $scope = self.$$scope;

    (function($base) {
      var self = $module($base, 'CSS');

      var def = self.$$proto, $scope = self.$$scope;

      (function($base, $super) {
        function $Rule(){};
        var self = $Rule = $klass($base, $super, 'Rule', $Rule);

        var def = self.$$proto, $scope = self.$$scope;

        return (function($base, $super) {
          function $Style(){};
          var self = $Style = $klass($base, $super, 'Style', $Style);

          var def = self.$$proto, $scope = self.$$scope, TMP_1;

          def["native"] = nil;
          self.$alias_native("selector", "selectorText");

          self.$alias_native("id", "selectorText");

          def.$declaration = function() {
            var self = this;

            return $scope.get('Declaration').$new(self["native"].style);
          };

          return (def.$method_missing = TMP_1 = function(args) {
            var $a, $b, self = this, $iter = TMP_1.$$p, block = $iter || nil;

            args = $slice.call(arguments, 0);
            TMP_1.$$p = null;
            return ($a = ($b = self.$declaration()).$__send__, $a.$$p = block.$to_proc(), $a).apply($b, [].concat(args));
          }, nil) && 'method_missing';
        })(self, $scope.get('Rule'))
      })(self, null)
    })(self)
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser/css"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $gvars = Opal.gvars;

  Opal.add_stubs(['$require', '$create_element', '$[]=', '$inner_text=', '$css', '$to_proc']);
  self.$require("browser/css/declaration");
  self.$require("browser/css/style_sheet");
  self.$require("browser/css/rule");
  self.$require("browser/css/rule/style");
  return (function($base) {
    var self = $module($base, 'Kernel');

    var def = self.$$proto, $scope = self.$$scope, TMP_1;

    Opal.defn(self, '$CSS', TMP_1 = function(text) {
      var $a, $b, $c, $d, self = this, $iter = TMP_1.$$p, block = $iter || nil, style = nil;
      if ($gvars.document == null) $gvars.document = nil;

      if (text == null) {
        text = nil
      }
      TMP_1.$$p = null;
      style = $gvars.document.$create_element("style");
      style['$[]=']("type", "text/css");
      if (block !== false && block !== nil) {
        (($a = [($c = ($d = $scope.get('Paggio')).$css, $c.$$p = block.$to_proc(), $c).call($d)]), $b = style, $b['$inner_text='].apply($b, $a), $a[$a.length-1])
        } else {
        (($a = [text]), $b = style, $b['$inner_text='].apply($b, $a), $a[$a.length-1])
      };
      return style;
    })
  })(self);
};

/* Generated by Opal 0.7.2 */
Opal.modules["browser"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice;

  Opal.add_stubs(['$require']);
  self.$require("native");
  self.$require("paggio");
  self.$require("browser/version");
  self.$require("browser/utils");
  self.$require("browser/compatibility");
  self.$require("browser/window");
  self.$require("browser/dom");
  return self.$require("browser/css");
};

/* Generated by Opal 0.7.2 */
Opal.modules["snake"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass, $hash2 = Opal.hash2;

  Opal.add_stubs(['$attr_reader', '$new', '$+', '$<<', '$==', '$direction', '$head', '$[]', '$last', '$square', '$[]=', '$coords', '$puts', '$first', '$shift', '$y', '$x', '$each', '$move', '$update_square', '$generate_bone', '$unfood!', '$private', '$check_for_changes', '$add_food!']);
  return (function($base, $super) {
    function $Snake(){};
    var self = $Snake = $klass($base, $super, 'Snake', $Snake);

    var def = self.$$proto, $scope = self.$$scope;

    def.bones = def.changes = def.changes_cache = def.to_add = nil;
    self.$attr_reader("bones", "lost", "changes", "changes_cache", "to_add");

    def.$initialize = function() {
      var $a, $b, TMP_1, self = this;

      self.bones = [];
      self.lost = false;
      self.changes = [];
      self.changes_cache = ($a = ($b = $scope.get('Array')).$new, $a.$$p = (TMP_1 = function(){var self = TMP_1.$$s || this;

      return $scope.get('Array').$new((($scope.get('Board')).$$scope.get('SIZE'))['$+'](1), nil)}, TMP_1.$$s = self, TMP_1), $a).call($b, (($scope.get('Board')).$$scope.get('SIZE'))['$+'](1));
      return self.to_add = nil;
    };

    def.$add_bone = function(bone) {
      var self = this;

      return self.bones['$<<'](bone);
    };

    def.$add_change = function(direction) {
      var $a, $b, self = this, change = nil;

      if (direction['$=='](self.$head().$direction())) {
        return nil};
      if (direction['$==']((($scope.get('Board')).$$scope.get('OPPOSITE_DIRECTION'))['$[]'](self.$head().$direction()))) {
        return nil};
      if ((($a = ($b = self.changes.$last(), $b !== false && $b !== nil ?self.changes.$last().$square()['$=='](self.$head().$square()) : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return nil};
      change = $scope.get('Change').$new(self.$head().$square(), direction);
      self.changes['$<<'](change);
      self.changes_cache['$[]'](self.$head().$coords()['$[]']("y"))['$[]='](self.$head().$coords()['$[]']("x"), change);
      return self.$puts("Added change direction: " + (direction));
    };

    def.$head = function() {
      var self = this;

      return self.bones.$first();
    };

    def.$last = function() {
      var self = this;

      return self.bones.$last();
    };

    def.$remove_last_change = function() {
      var self = this, change = nil;

      change = self.changes.$shift();
      return self.changes_cache['$[]'](change.$y())['$[]='](change.$x(), nil);
    };

    def['$move!'] = function() {try {

      var $a, $b, TMP_2, $c, TMP_3, self = this;

      ($a = ($b = self.bones).$each, $a.$$p = (TMP_2 = function(b){var self = TMP_2.$$s || this, $a;
        if (self.lost == null) self.lost = nil;
if (b == null) b = nil;
      b.$move();
        if ((($a = self.lost) !== nil && (!$a.$$is_boolean || $a == true))) {
          Opal.ret(nil)
          } else {
          return nil
        };}, TMP_2.$$s = self, TMP_2), $a).call($b);
      ($a = ($c = self.bones).$each, $a.$$p = (TMP_3 = function(b){var self = TMP_3.$$s || this;
if (b == null) b = nil;
      return b.$update_square()}, TMP_3.$$s = self, TMP_3), $a).call($c);
      if ((($a = self.to_add) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self.$generate_bone()
        } else {
        return nil
      };
      } catch ($returner) { if ($returner === Opal.returner) { return $returner.$v } throw $returner; }
    };

    def['$lose!'] = function() {
      var self = this;

      return self.lost = true;
    };

    def['$eat!'] = function(square) {
      var self = this;

      square['$unfood!']();
      return self.to_add = $hash2(["square", "direction"], {"square": self.$last().$square(), "direction": self.$last().$direction()});
    };

    self.$private();

    return (def.$generate_bone = function() {
      var self = this, bone = nil;

      bone = $scope.get('Bone').$new(self, self.to_add['$[]']("square"), self.to_add['$[]']("direction"));
      self.to_add = nil;
      bone.$check_for_changes();
      return $scope.get('Board')['$add_food!']();
    }, nil) && 'generate_bone';
  })(self, null)
};

/* Generated by Opal 0.7.2 */
Opal.modules["board"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $module = Opal.module, $hash2 = Opal.hash2;

  Opal.add_stubs(['$extend', '$prepare_matrix', '$===', '$==', '$find', '$[]', '$<', '$>', '$snake', '$each', '$!', '$push', '$sample', '$food!', '$random_square', '$private', '$upto', '$[]=', '$new']);
  return (function($base) {
    var self = $module($base, 'Board');

    var def = self.$$proto, $scope = self.$$scope;

    Opal.cdecl($scope, 'OPPOSITE_DIRECTION', $hash2(["left", "right", "up", "down"], {"left": "right", "right": "left", "up": "down", "down": "up"}));

    Opal.cdecl($scope, 'SIZE', 50);

    self.$extend(self);

    Opal.defn(self, '$generate', function() {
      var self = this;

      return self.$prepare_matrix();
    });

    Opal.defn(self, '$find', function(x, y) {
      var $a, $b, self = this;

      if (y == null) {
        y = 0
      }
      if ((($a = ($b = $scope.get('Hash')['$==='](x), $b !== false && $b !== nil ?y['$=='](0) : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self.$find(x['$[]']("x"), x['$[]']("y"))};
      if ((($a = ((($b = x['$<'](1)) !== false && $b !== nil) ? $b : y['$<'](1))) !== nil && (!$a.$$is_boolean || $a == true))) {
        return "not_in_board"};
      if ((($a = ((($b = x['$>']($scope.get('SIZE'))) !== false && $b !== nil) ? $b : y['$>']($scope.get('SIZE')))) !== nil && (!$a.$$is_boolean || $a == true))) {
        return "not_in_board"};
      return (($a = Opal.cvars['@@matrix']) == null ? nil : $a)['$[]'](y)['$[]'](x);
    });

    Opal.defn(self, '$matrix', function() {
      var $a, self = this;

      return (($a = Opal.cvars['@@matrix']) == null ? nil : $a);
    });

    Opal.defn(self, '$part_of_snake?', function(x, y) {
      var self = this;

      return self.$find(x, y).$snake();
    });

    Opal.defn(self, '$random_square', function() {
      var $a, $b, TMP_1, $c, self = this, squares = nil;

      squares = [];
      ($a = ($b = (($c = Opal.cvars['@@matrix']) == null ? nil : $c)).$each, $a.$$p = (TMP_1 = function(row){var self = TMP_1.$$s || this, $a, $b, TMP_2;
if (row == null) row = nil;
      if (row !== false && row !== nil) {
          } else {
          return nil;
        };
        return ($a = ($b = row).$each, $a.$$p = (TMP_2 = function(square){var self = TMP_2.$$s || this, $a, $b;
if (square == null) square = nil;
        if ((($a = (($b = square !== false && square !== nil) ? square.$snake()['$!']() : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
            return squares.$push(square)
            } else {
            return nil
          }}, TMP_2.$$s = self, TMP_2), $a).call($b);}, TMP_1.$$s = self, TMP_1), $a).call($b);
      return squares.$sample();
    });

    Opal.defn(self, '$add_food!', function() {
      var self = this;

      return self.$random_square()['$food!']();
    });

    self.$private();

    Opal.defn(self, '$prepare_matrix', function() {
      var $a, $b, TMP_3, self = this;

      (Opal.cvars['@@matrix'] = []);
      return ($a = ($b = (1)).$upto, $a.$$p = (TMP_3 = function(y){var self = TMP_3.$$s || this, $a, $b, TMP_4;
if (y == null) y = nil;
      (($a = Opal.cvars['@@matrix']) == null ? nil : $a)['$[]='](y, []);
        return ($a = ($b = (1)).$upto, $a.$$p = (TMP_4 = function(x){var self = TMP_4.$$s || this, $a;
if (x == null) x = nil;
        return (($a = Opal.cvars['@@matrix']) == null ? nil : $a)['$[]'](y)['$[]='](x, $scope.get('Square').$new(x, y, $scope.get('Element').$find(".square-" + (x) + "-" + (y))))}, TMP_4.$$s = self, TMP_4), $a).call($b, $scope.get('SIZE'));}, TMP_3.$$s = self, TMP_3), $a).call($b, $scope.get('SIZE'));
    });
  })(self)
};

/* Generated by Opal 0.7.2 */
Opal.modules["square"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass, $hash2 = Opal.hash2;

  Opal.add_stubs(['$attr_reader', '$attr_accessor', '$x', '$y', '$add_class', '$remove_class']);
  return (function($base, $super) {
    function $Square(){};
    var self = $Square = $klass($base, $super, 'Square', $Square);

    var def = self.$$proto, $scope = self.$$scope;

    def.snake = def.div = nil;
    self.$attr_reader("x", "y", "div");

    self.$attr_accessor("snake", "food");

    def.$initialize = function(x, y, div) {
      var self = this;

      if (div == null) {
        div = nil
      }
      self.x = x;
      self.y = y;
      self.div = div;
      self.snake = false;
      return self.food = false;
    };

    def['$snake?'] = function() {
      var self = this;

      return self.snake;
    };

    def.$coords = function() {
      var self = this;

      return $hash2(["x", "y"], {"x": self.$x(), "y": self.$y()});
    };

    def.$add_bone_class = function() {
      var self = this;

      self.div.$add_class("bone");
      return self.snake = true;
    };

    def.$remove_bone_class = function() {
      var self = this;

      self.div.$remove_class("bone");
      return self.snake = false;
    };

    def['$food!'] = function() {
      var self = this;

      self.div.$add_class("food");
      return self.food = true;
    };

    return (def['$unfood!'] = function() {
      var self = this;

      self.div.$remove_class("food");
      return self.food = false;
    }, nil) && 'unfood!';
  })(self, null)
};

/* Generated by Opal 0.7.2 */
Opal.modules["bone"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass, $hash2 = Opal.hash2;

  Opal.add_stubs(['$attr_reader', '$add_bone_class', '$empty?', '$bones', '$add_class', '$div', '$add_bone', '$check_for_changes', '$find', '$new_coords', '$==', '$lose!', '$snake', '$!', '$square', '$last', '$food', '$eat!', '$remove_bone_class', '$change_head', '$coords', '$[]', '$+', '$x', '$y', '$changes_cache', '$direction', '$to_add', '$remove_last_change', '$private', '$remove_class']);
  return (function($base, $super) {
    function $Bone(){};
    var self = $Bone = $klass($base, $super, 'Bone', $Bone);

    var def = self.$$proto, $scope = self.$$scope;

    def.square = def.snake = def.head = def.old_square = def.direction = nil;
    self.$attr_reader("snake", "square", "direction");

    Opal.cdecl($scope, 'MOVES', $hash2(["up", "down", "left", "right"], {"up": $hash2(["x", "y"], {"x": 0, "y": -1}), "down": $hash2(["x", "y"], {"x": 0, "y": 1}), "left": $hash2(["x", "y"], {"x": -1, "y": 0}), "right": $hash2(["x", "y"], {"x": 1, "y": 0})}));

    def.$initialize = function(snake, square, direction) {
      var $a, self = this;

      self.snake = snake;
      self.square = square;
      self.square.$add_bone_class();
      self.direction = direction;
      self.old_square = nil;
      if ((($a = snake.$bones()['$empty?']()) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.head = true;
        self.square.$div().$add_class("head");
        } else {
        self.head = false
      };
      return self.snake.$add_bone(self);
    };

    def.$move = function() {
      var $a, $b, $c, self = this;

      self.$check_for_changes();
      self.old_square = self.square;
      self.square = $scope.get('Board').$find(self.$new_coords());
      if (self.square['$==']("not_in_board")) {
        return self.snake['$lose!']()
      } else if ((($a = ($b = ($c = self.head, $c !== false && $c !== nil ?self.square.$snake() : $c), $b !== false && $b !== nil ?self.square['$=='](self.$snake().$last().$square())['$!']() : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self.snake['$lose!']()
      } else if ((($a = ($b = self.head, $b !== false && $b !== nil ?self.square.$food() : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self.snake['$eat!'](self.square)
        } else {
        return nil
      };
    };

    def.$update_square = function() {
      var $a, $b, self = this;

      self.square.$add_bone_class();
      if ((($a = ($b = self.old_square, $b !== false && $b !== nil ?self['$=='](self.snake.$last()) : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.old_square.$remove_bone_class()};
      if ((($a = self.head) !== nil && (!$a.$$is_boolean || $a == true))) {
        return self.$change_head()
        } else {
        return nil
      };
    };

    def.$coords = function() {
      var self = this;

      return self.square.$coords();
    };

    def.$new_coords = function() {
      var self = this, dest = nil;

      dest = $scope.get('MOVES')['$[]'](self.direction);
      return $hash2(["x", "y"], {"x": self.square.$x()['$+'](dest['$[]']("x")), "y": self.square.$y()['$+'](dest['$[]']("y"))});
    };

    def['$head?'] = function() {
      var self = this;

      return self.head;
    };

    def.$check_for_changes = function() {
      var $a, $b, self = this, c = nil;

      if ((($a = c = self.snake.$changes_cache()['$[]'](self.$square().$y())['$[]'](self.$square().$x())) !== nil && (!$a.$$is_boolean || $a == true))) {
        self.direction = c.$direction();
        if ((($a = (($b = self['$=='](self.snake.$last())) ? self.snake.$to_add()['$!']() : $b)) !== nil && (!$a.$$is_boolean || $a == true))) {
          return self.snake.$remove_last_change()
          } else {
          return nil
        };
        } else {
        return nil
      };
    };

    self.$private();

    return (def.$change_head = function() {
      var self = this;

      self.old_square.$div().$remove_class("head");
      return self.square.$div().$add_class("head");
    }, nil) && 'change_head';
  })(self, null)
};

/* Generated by Opal 0.7.2 */
Opal.modules["change"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, $klass = Opal.klass;

  Opal.add_stubs(['$attr_reader', '$x', '$y', '$coords']);
  return (function($base, $super) {
    function $Change(){};
    var self = $Change = $klass($base, $super, 'Change', $Change);

    var def = self.$$proto, $scope = self.$$scope;

    def.square = nil;
    self.$attr_reader("square", "direction");

    def.$initialize = function(square, direction) {
      var self = this;

      self.square = square;
      return self.direction = direction;
    };

    def.$x = function() {
      var self = this;

      return self.square.$x();
    };

    def.$y = function() {
      var self = this;

      return self.square.$y();
    };

    return (def.$coords = function() {
      var self = this;

      return self.square.$coords();
    }, nil) && 'coords';
  })(self, null)
};

/* Generated by Opal 0.7.2 */
Opal.modules["main"] = function(Opal) {
  Opal.dynamic_require_severity = "error";
  var $a, $b, TMP_1, self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice, snake = nil;

  Opal.add_stubs(['$new', '$ready?', '$generate', '$find', '$on', '$to_i', '$key_code', '$===', '$prevent_default', '$add_change', '$move!', '$add_food!', '$every', '$lost', '$abort']);
  snake = $scope.get('Snake').$new();
  return ($a = ($b = $scope.get('Document'))['$ready?'], $a.$$p = (TMP_1 = function(){var self = TMP_1.$$s || this, $a, $b, TMP_2, $c, TMP_3, main_loop = nil;

  $scope.get('Board').$generate();
    $scope.get('Bone').$new(snake, $scope.get('Board').$find(20, 20), "left");
    $scope.get('Bone').$new(snake, $scope.get('Board').$find(21, 20), "left");
    $scope.get('Bone').$new(snake, $scope.get('Board').$find(22, 20), "left");
    $scope.get('Bone').$new(snake, $scope.get('Board').$find(23, 20), "left");
    $scope.get('Bone').$new(snake, $scope.get('Board').$find(24, 20), "left");
    $scope.get('Bone').$new(snake, $scope.get('Board').$find(25, 20), "left");
    ($a = ($b = $scope.get('Document')).$on, $a.$$p = (TMP_2 = function(e){var self = TMP_2.$$s || this, $case = nil;
if (e == null) e = nil;
    return (function() {$case = e.$key_code().$to_i();if ((38)['$===']($case)) {e.$prevent_default();
      return snake.$add_change("up");}else if ((40)['$===']($case)) {e.$prevent_default();
      return snake.$add_change("down");}else if ((37)['$===']($case)) {e.$prevent_default();
      return snake.$add_change("left");}else if ((39)['$===']($case)) {e.$prevent_default();
      return snake.$add_change("right");}else { return nil }})()}, TMP_2.$$s = self, TMP_2), $a).call($b, "keydown");
    snake['$move!']();
    $scope.get('Board')['$add_food!']();
    return main_loop = ($a = ($c = self).$every, $a.$$p = (TMP_3 = function(){var self = TMP_3.$$s || this, $a;

    snake['$move!']();
      if ((($a = snake.$lost()) !== nil && (!$a.$$is_boolean || $a == true))) {
        return main_loop.$abort()
        } else {
        return nil
      };}, TMP_3.$$s = self, TMP_3), $a).call($c, 0.1);}, TMP_1.$$s = self, TMP_1), $a).call($b);
};

/* Generated by Opal 0.7.2 */
(function(Opal) {
  Opal.dynamic_require_severity = "error";
  var self = Opal.top, $scope = Opal, nil = Opal.nil, $breaker = Opal.breaker, $slice = Opal.slice;

  Opal.add_stubs(['$require']);
  self.$require("opal");
  self.$require("opal-jquery");
  self.$require("browser");
  self.$require("snake");
  self.$require("board");
  self.$require("square");
  self.$require("bone");
  self.$require("change");
  return self.$require("main");
})(Opal);
