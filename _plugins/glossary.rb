require 'nokogiri'
require 'cgi'

module GlossaryData
  def glossary
    self['glossary']
  end
end

module ChaptersData
  def chapters
    self['chapters']
  end
end

module Jekyll
  class GlossaryLinkGenerator < Generator
    safe true
    priority :low

    @@glossary_terms = {}

    def generate(site)
      site.data.extend(GlossaryData)
      site.data.extend(ChaptersData)

      @@titles = site.data.chapters.map { |chapter| chapter['url'].gsub(/^\/|\/$/, '') }

      site.data.glossary.each do |entry|
        term = entry['term'].downcase
        definition = Kramdown::Document.new(entry['definition']).to_html.gsub(/<\/?p>/, '').gsub(/<a.*?>(.+?)<\/a>/, '$1')
        first_appeared = entry['first_appeared']

        @@glossary_terms[term] = [definition, first_appeared]
        @@glossary_terms[pluralize(term)] = [definition, first_appeared]

      end

      @@glossary_term_regex ||= /(#{@@glossary_terms.map { |term, _| "\\b#{Regexp.escape(term)}\\b" }.join('|')})(?![^<]*<\/span>)/i
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
        file_name = File.basename(document.relative_path, File.extname(document.relative_path))
        document.output = replace_glossary_terms(document.output, file_name)
      end
    end

    def self.replace_glossary_terms(content, file_name)
      doc = Nokogiri::HTML5.parse(content)

      doc.traverse do |node|
        if node.text?
          unless node.ancestors.any? { |ancestor| ancestor.name.match?(/^h[1-6]$/i) || ancestor.name == 'pre' || ancestor.name == 'blockquote' || ancestor['class'] == 'glossary' || ancestor.name == 'code' || ancestor['class'] == 'glossary-term' || ancestor.name == 'title' }
            new_content = node.content.gsub(@@glossary_term_regex) do |match|
              term = match.downcase
              definition, first_appeared = @@glossary_terms[term]
              if @@titles.include?(file_name) and @@titles.index(file_name) >= @@titles.index(first_appeared)
                "<span class='glossary-term' data-term='#{term}'>#{match}<span class='glossary-popup'>#{definition}</span></span>"
              else
                match
              end
            end
            node.replace(Nokogiri::HTML5.fragment(new_content))
          end
        end
      end

      doc.to_html
    end

  end
end
