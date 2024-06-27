module Jekyll
    class WrapSolutions < Jekyll::Generator
      def generate(site)
        # Process regular pages
        site.pages.each do |page|
          if page.extname == ".md"
            process_page(page)
          end
        end
  
        # Process documents in collections
        site.collections.each do |_label, collection|
          collection.docs.each do |doc|
            if doc.extname == ".md"
              process_page(doc)
            end
          end
        end
      end
  
      def process_page(page)        
        processed_content = wrap_solutions(page.content)
        processed_content = wrap_glossary(processed_content)
        page.content = processed_content
      end
  
      def wrap_solutions(content)
        content.gsub(/(##+ *Solutions.*?)(?=\n##+ |\z)/m) do |match|
          "{% capture solution_content %}\n#{match}\n{% endcapture %}\n{% include solutions.html solution_content=solution_content %}"
        end        
      end
      def wrap_glossary(content)
        content.gsub(/(##+ *Glossary.*)(?=\z)/m) do |match|
          "<div class=\"glossary\" markdown=\"1\">\n#{match}\n</div>"
        end      
      end
    end
  end