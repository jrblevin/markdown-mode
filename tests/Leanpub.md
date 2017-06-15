# Leanpub

{frontmatter}

## Section {#id}

[Link to section](#id)

{mainmatter}

## Aside Blocks

A> ## Your Aside Title {#your-aside-title}
A>
A> This is also sometimes known as a sidebar.
A> More info would go here.

C> Centered text

W> Text in a warning box

T> Text in a tip box

E> Text in an error box

I> Text in an information box

Q> Text in a question box

D> Text in a discussion box

X> Text in an exercise box

{pagebreak}

General blocks:

{icon=automobile}
G> ## Vrooom!
G>
G> The freedom of the road!

## Code Blocks

Here is a code sample:

<<(2017-05-04.txt)

<<[This Code Sample Has A Title](code/sample2.rb)

{title="Example 12: Creating a class in Ruby", .class, lang=ruby}
~~~~~~~
class Book

  def initialize(title, subtitle)
    @title = title
    @subtitle = subtitle
  end

end
~~~~~~~
