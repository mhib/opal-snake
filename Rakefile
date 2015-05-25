# Rakefile
Bundler.require

Opal.append_path "app"

desc "Build our app to build.js"
task :build do
  File.binwrite "build.js", Opal::Builder.build("application").to_s
end

require 'opal/rspec/rake_task'
Opal::RSpec::RakeTask.new(:default) do |s|
  s.index_path = 'spec/jquery/index.html.erb'
end
