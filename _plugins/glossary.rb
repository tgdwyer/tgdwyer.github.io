require 'nokogiri'

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
      Jekyll.logger.info "GlossaryLinkGenerator:", "Starting..."

      site.data.extend(GlossaryData)

      site.data.glossary.each do |entry|
        term = entry['term'].downcase
        definition = entry['definition']
        @@glossary_terms[term] = definition
        # Add plural forms
        @@glossary_terms[pluralize(term)] = definition
      end

      # sort the list, to priorties longer definitions.
      @@glossary_terms = @@glossary_terms.sort_by { |term, _| -term.length }.to_h

      Jekyll.logger.info "GlossaryLinkGenerator:", "Glossary terms loaded: #{@@glossary_terms.keys.join(', ')}"
    end

    def pluralize(term)
      # A simple pluralization method
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
        Jekyll.logger.info "GlossaryLinkGenerator:", "Processing document: #{document.relative_path}"

        @@glossary_terms.each do |term, definition|
          regex = /(\b#{Regexp.escape(term)}\b)/i
          document.output = replace_glossary_terms(document.output, regex, term, definition)
        end
      end
    end

    def self.replace_glossary_terms(content, regex, term, definition)
      doc = Nokogiri::HTML.fragment(content)

      doc.traverse do |node|
        if node.text?
          unless node.ancestors.any? { |ancestor| ancestor.name.match?(/^h[1-6]$/i) || ancestor.name == 'pre' || ancestor['class'] == 'glossary' || ancestor.name == 'code' || ancestor['class'] == "glossary-term" || ancestor.name == "title" }
            new_content = node.content.gsub(regex) do |match|
              Jekyll.logger.info "GlossaryLinkGenerator:", "Adding popup for term '#{match}'"
              "<span class='glossary-term' data-term='#{term}' data-definition='#{definition}'>#{match}</span>"
            end
            node.replace(Nokogiri::HTML.fragment(new_content))
          end
        end
      end

      doc.to_html
    end
  end
end
