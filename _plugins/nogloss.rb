require 'liquid'

module Jekyll
  class NoGlossTag < Liquid::Tag
    def initialize(tag_name, text, tokens)
      super
      @text = text.strip
    end

    def render(context)
      %(<span data-no-glossary="true">#{@text}</span>)
    end
  end
end

Liquid::Template.register_tag('nogloss', Jekyll::NoGlossTag)
