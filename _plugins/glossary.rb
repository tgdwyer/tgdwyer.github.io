require 'nokogiri'
require 'cgi'

module GlossaryData
  def glossary
    self['glossary']
  end
end

module Jekyll
  class GlossaryLinkGenerator < Generator
    safe true
    priority :low

    @@glossary_terms = {}

    def generate(site)
      site.data.extend(GlossaryData)

      site.data.glossary.each do |entry|
        term = entry['term'].downcase
        definition = entry['definition']
        @@glossary_terms[term] = definition
        @@glossary_terms[pluralize(term)] = definition
      end

      @@glossary_terms = @@glossary_terms.sort_by { |term, _| -term.length }.to_h

    end

    def pluralize(term)
      # A simple pluralization method
      # Developed by ChatGPT, since grammar is hard.
      if term.end_with?('y')
        term[0..-2] + 'ies'
      elsif term.end_with?('s')
        term + 'es'
      else
        term + 's'
      end
    end

    Jekyll::Hooks.register :documents, :post_render do |document|
      if document.output_ext == '.html'
        @@compiled_glossary_terms ||= @@glossary_terms.map do |term, definition|
          regex = Regexp.new(/(\b#{Regexp.escape(term)}\b)(?![^<]*<\/span>)/i)
          [term, CGI.escapeHTML(definition), regex]
        end
        
        document.output = replace_glossary_terms(document.output)
    
      end
    end

    def self.replace_glossary_terms(content)
      doc = Nokogiri::HTML.fragment(content)
    
      doc.traverse do |node|
        if node.text?
          unless node.ancestors.any? { |ancestor| ancestor.name.match?(/^h[1-6]$/i) || ancestor.name == 'pre' || ancestor['class'] == 'glossary' || ancestor.name == 'code' || ancestor['class'] == 'glossary-term' || ancestor.name == 'title' }
            new_content = node.content
            @@compiled_glossary_terms.each do |term, definition, regex|
              new_content = new_content.gsub(regex) do |match|
                "<span class='glossary-term' data-term='#{term}' data-definition='#{definition}'>#{match}</span>"
              end
            end
            node.replace(Nokogiri::HTML.fragment(new_content))
          end        
        end
      end
    
      doc.to_html
    end
    
  end
end
