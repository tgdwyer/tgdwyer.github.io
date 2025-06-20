# tgdwyer.github.io

Jekyll source for [my notes on Programming Paradigms](https://tgdwyer.github.io/)

To build offline, [install Jekyll](https://jekyllrb.com/docs/installation/), then:

```sh
bundle exec jekyll serve
```

## Contributing

We use pre-commit to ensure quality and consistency of the markdown. To contribute, please follow these steps:

1. Install pre-commit:

   ```sh
   pip install pre-commit
   ```

2. Install the pre-commit hooks:

   ```sh
   pre-commit install
   ```

After this, the pre-commit hooks will run automatically on each commit to check for spelling and linting.

### Adding Solutions

If you want to add solutions to your markdown files in this Jekyll site, you need to separate the solutions into a section. Ensure that your solutions are marked with a heading that is only the word "Solutions". This can be at any heading level (e.g., `### Solutions`, `#### Solutions`). For example:

```markdown
### Solutions

Your solution content goes here...

```javascript
const exampleFunction = () => {
  console.log("This is an example solution");
};

## Rest of Document

```

Do not include any subheadings in solutions. The `wrap_solution` plugin, will automatically process this and hide solutions by default, and will be toggleable on any relevant page.

### Adding Alert Boxes

To add an alert box:

```markdown
<div class="alert-box alert-info" markdown="1">
This is an *important* bit of information!
</div>
```

Supported alert boxes are `alert-info`, `alert-warning`, and `alert-danger`. To add a heading to the alert box, put the heading in strong emphasis/bold:

```markdown
<div class="alert-box alert-info" markdown="1">
**Important Information**
This is an *important* bit of information!
</div>
```

The CSS styles will automatically add ‘💡 Note:’, ‘⚠️ Warning:’, or ‘🚨 Important:’ (for the classes `alert-info`, `alert-warning`, and `alert-danger` respectively) in front of the text in bold. Note that this means you cannot use bold text anywhere else in the info box at the moment.
