# require 'nokogiri'

# module GlossaryData
#   def glossary
#     self['glossary']
#   end
# end

# module Jekyll
#   class GlossaryLinkGenerator < Generator
#     safe true
#     priority :low

#     def generate(site)
#       Jekyll.logger.info "GlossaryLinkGenerator:", "Starting..."

#       site.data.extend(GlossaryData)

#       glossary_terms = {}
#       site.data.glossary.each do |entry|
#         glossary_terms[entry['term'].downcase] = entry['definition']
#         glossary_terms[pluralize(entry['term'].downcase)] = entry['definition']
#       end

#       site.documents.each do |document|
#         if document.output_ext == '.html'

#           glossary_terms.each do |term, definition|
#             regex = /(\b#{Regexp.escape(term)}\b)/i
#             document.content = replace_glossary_terms(document.content, regex, term, definition)
#           end
#         end
#       end
    

#     end

#     def replace_glossary_terms(content, regex, term, definition)
#       # segments = content.split(/(<a[^>]*>.*?<\/a>|<span[^>]*>.*?<\/span>|<h\d[^>]*>.*?<\/h\d>)/i).map do |segment|
#       #   if segment =~ /<a[^>]*>.*?<\/a>|<span[^>]*>.*?<\/span>|<h\d[^>]*>.*?<\/h\d>/i
#       #     segment
#       #   else
#       #     segment.gsub(regex) do |match|
#       #       # "<span class='glossary-term' data-term='#{term}' data-definition='#{definition}'>#{match}</span>"
#       #       %(<span class="glossary-term" data-term="#{escape_html(term)}" data-definition="#{escape_html(definition)}">#{match}</span>)
#       #     end
#       #   end
#       # end.join
#       doc = Nokogiri::HTML.fragment(content)
      
#       doc.traverse do |node|
#         if node.text?
#           puts "Node content: #{node.content.strip}"
#           puts "Ancestors: #{node.ancestors.map { |ancestor| ancestor.name }.join(', ')}"
#           puts "Matches heading: #{node.ancestors.any? { |ancestor| ancestor.name.match?(/^h[1-6]$/i) }}"
#           puts "-----------------"

#           unless node.ancestors.any? { |ancestor| ancestor.name.match?(/^h[1-6]$/i) }
#             # Jekyll.logger.info "brrr"
#             new_content = node.content.gsub(regex) do |match|
#               Jekyll.logger.info "GlossaryLinkGenerator:", "Adding popup for term '#{match}'"
#               "<span class='glossary-term' data-term='#{term}' data-definition='#{definition}'>#{match}</span>"
#             end
#             node.replace(Nokogiri::HTML.fragment(new_content))
#           end
#       end

#       end

#       doc.to_html

#     end

#     def pluralize(term)
#       # A simple pluralization method
#       if term.end_with?('y')
#         term[0..-2] + 'ies'
#       elsif term.end_with?('s')
#         term + 'es'
#       else
#         term + 's'
#       end
#     end
#     def escape_html(text)
#       CGI.escapeHTML(text)
#     end


#   end
# end
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
          # unless node.ancestors.any? { |ancestor| ancestor.name.match?(/^h[1-6]$/i) }
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
