input_format=markdown_github-hard_line_breaks

# Generate combined markdown document for README.md
pandoc --from="$input_format" \
       --to=markdown_github-raw_html \
       --output=../README.md \
       --standalone \
       --template=template.readme \
       --toc \
       features.md \
       reference-syntax.md \
       reference-units.md \
       pros-and-cons.md \
       faq.md \
       terminal-version.md \
       development.md

# Generate manpage
pandoc --from="$input_format" \
       --to=man \
       --output=insect.1 \
       --standalone \
       --reference-links \
       -V section=1 \
       -V header="insect - scientific calculator" \
       manpage-header.md \
       features.md \
       reference-syntax.md \
       faq.md \
       manpage-footer.md
