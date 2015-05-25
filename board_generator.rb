#SIZE = 50
SIZE = 5
str = ""
1.upto(SIZE) do |y|
  str << %(<div class="row">\n)
  1.upto(SIZE) do |x|
    str << %(<div class="square square-#{x}-#{y}" data-x="#{x}" data-y="#{y}">\n)
    str << "</div>\n"
  end
  str << "</div>\n"
end
puts str
