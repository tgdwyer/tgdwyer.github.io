---
layout: chapter
title: "HTML as a Declarative Language"
---

## Learning Outcomes

- Understand what makes HTML a declarative language and how it describes the structure and content of a web page.
- Comprehend the separation of concerns by differentiating structure (HTML), presentation (CSS), and behavior (JavaScript).
- Understand how to include and define a basic SVG element within an HTML document.

## What is HTML?

HTML, or HyperText Markup Language, is the standard markup language used to create and design web pages. It provides the structure and content of a web page by using a system of markup tags and attributes. HTML documents are interpreted by web browsers to render the content visually for users.

## Declarative Nature of HTML

HTML is considered a declarative language because it focuses on describing the structure and content of a web page without specifying how to achieve it. Instead of giving step-by-step instructions for rendering elements, HTML allows developers to declare the desired structure and let the browser handle the rendering process.

## Key Aspects of HTML's Declarative Nature

1. Descriptive Tags: HTML tags are descriptive elements that define the purpose and meaning of content. For example, `<p>` tags indicate a paragraph, `<h1>` to `<h6>` tags denote headings of varying levels, `<ul>` and `<ol>` represent unordered and ordered lists respectively. These tags describe the content they enclose rather than instructing how it should be displayed.  Pairs of opening and closing HTML tags (e.g. `<h1>` defines the start of a heading, `</h1>` marks the end) define *elements* in a Document Object Model (DOM).  Elements can be nested within each other such that the DOM is a hierarchical (tree) structure.  
2. Attribute-Based: HTML elements can have attributes that provide additional information or functionality. Attributes like class, id, src, href, etc., provide hooks for styling, scripting, or specifying behavior. However, these attributes do not dictate how elements are displayed; they simply provide metadata or instructions to browsers.

3. Separation of Concerns: Modern HTML5 is actually a suite of languages which encourages a separation of concerns by delineating structure (HTML), presentation (CSS), and behavior (JavaScript). This promotes maintainability and scalability by allowing each aspect of web development to be managed independently.

4. Browser Interpretation: HTML documents are interpreted by web browsers, which render the content based on the instructions provided in the markup. Browsers apply default styles and layout algorithms to HTML elements, ensuring consistency across different platforms and devices.

## An Animated Rectangle Using SVG in HTML

### Step 1: Setting Up the HTML Document

First, create a new HTML file and define the basic structure of the document:

```html
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <title>Moving Rectangle Tutorial</title>
</head>
<body>

</body>
</html>
```

In this step, we've set up the basic HTML structure with a `<!DOCTYPE>` declaration, `<html>`, `<head>`, and `<body>` tags. We've also included meta tags for character encoding and viewport settings, as well as a title for the page.

### Step 2: Adding an SVG Element

Next, let's add an SVG element to the body of our HTML document. This SVG element will contain the rectangle that we'll animate later:

```html
<body>
  <svg width="100" height="100">
    <!-- SVG content will go here -->
  </svg>
</body>
```

We've added an SVG element with a width and height of 100 units each. This provides a canvas for our SVG graphics.

### Step 3: Adding a Rectangle to the SVG

Now, let's add a rectangle `<rect>` element inside the SVG to represent the moving rectangle:

```html
<svg width="100" height="100">
  <rect id="ourRectangle" x="10" y="10" width="20" height="20" fill="blue"/></svg>
```

In this step, we've defined a rectangle with a starting position at coordinates (10, 10) and a width and height of 20 units each. The rectangle is filled with a blue color.  Importantly, we've given the `<rect>` element a unique id "ourRectangle" by which we can refer to it elsewhere, below we'll demonstrate adding an animation behaviour to this rectangle using this id from CSS or JavaScript.

Most HTML elements, including SVG elements have certain attributes according to their documentation, which determine how they are rendered and behave in the browser. [MDN](https://developer.mozilla.org/en-US/docs/Web/SVG/Element/rect) is normally a good reference for what is available to use.

### Step 5: Adding Animation declaratively using CSS

There are many ways to achieve the same thing in HTML. We will now look at how an animation may be added *declaritively* to our SVG rectangle using CSS.  First, we need to tell the browser where to find our CSS file from the (<head>) element of our HTML.

```html
<head>
...
  <link rel="stylesheet" href="style.css" >
</head>
```

Now we create the `style.css` file as follows:

```css
#ourRectangle {  
    animation-name: moveX;
    animation-duration: 5s;
}

@keyframes moveX {
    0% {
        x: 0;
    }
    100% {
        x: 370;
    }
}
```

This first clause *selects* the rectangle by the unique id we gave it, and then declares some style properties. Specifically, it sets up on animation using "key frames" with a duration of 5 seconds.  In the keyframes declaration we can declare style properties for various time frames, and the browser will interpolate between them.  In this case, we have simply set an initial and final `x` position for the rectangle.

This is a program of sorts, but it's declarative in the sense that we did not tell the browser *how* to perform the animation.  Rather we *declared* what we wanted the rectangle to look like at the start and end of the animation and let the browser figure out how to perform the transition.

### Alternate Step 5: Adding a custom animation from Javascript

By contrast, we can create an *imperative* JavaScript program which explicitly gives the list of instructions for *how* to move the rectangle. First, remove the `<link>` to the CSS that we created in the `<head>` element of the html file.
Then can integrate a javascript by including a reference to a file, e.g., `script.js`

```html
<body>
  <svg width="100" height="100" id="svg">
    <rect x="10" y="10" width="20" height="20" fill="blue" id="ourRectangle"/>
  </svg>
  <script src="script.js"></script>
</body>
```

This will be a recursive function, which will animate the rectangle at 60 FPS. We use setTimeout to call our recursive function at around 60 frames per second. 

```javascript
// Define an animation function
function animate(rect, x, speed, lastTime) {
  const duration = 5000; // 5 seconds in milliseconds

  // Calculate elapsed time
  const currentTime = performance.now();
  const deltaTime = lastTime ? (currentTime - lastTime) : 0;

  // Check if animation duration has elapsed
  if (elapsedTime >= duration) {
    return; // Stop the animation
  }

  // Update position based on elapsed time and speed
  const newX = x + (speed * deltaTime) / 1000; // Convert milliseconds to seconds

  // We can use `setAttribute` to change the variables of the HTML Element. In this case, we are changing the x attribute. 
  rect.setAttribute('x', newX);

  // Set timeout to call the animate function again
  setTimeout(() => {
    animate(rect, newX, speed, currentTime);
  }, 1000 / 60); // 60 FPS
}

const rectangle = document.getElementById('ourRectangle')
// Start the animation
animate(rectangle, 10, 50, null);
```

However, there are some serious issues with this code.

- The rectangle attribute `x`, is a global variable, which the user of the webpage has access too!
  - If this was a game, the user could easily change this themselves to whatever value they please.
- If we wanted the duration to be very high, this could reach maximum recursion depth, which would cause the browser to crash
- We do not separate the state-management from where we cause the side-effects, which will increase difficulty when debugging.

Luckily, [functional reactive programming](/functionalreactiveprogramming) will save us from most of these issues!

## Conclusion

We saw that the philosophy of HTML programming is primarily *declarative* in the sense that the programmer/designer tells the browser what they want to see and rely on the browser's sophisticated rendering engine to figure out how to display the content.  This extends to adding dynamic behaviours such as animation declaratively through CSS.

By contrast, we saw a different *imperative* approach to adding an animation to our web page using JavaScript. There are pros and cons to each:

*Declarative*

- Easier and faster to code, easier to read
- Less likely to be buggy
- We spend less time on trying to figure out *how* to do things and focus on *what* we want

*Imperative*

- Greater control than the pure declarative HTML/CSS approach
- We can create richer interaction

## Exercises

- Modify Rectangle Properties: Experiment with changing the attributes of the rectangle such as width, height, fill color, and position. See how these changes affect the appearance of the rectangle on the screen.
- Animate Different Shapes: Besides rectangles, try animating other SVG shapes such as circles, ellipses, or polygons. Explore how the animation function can be adapted to work with different types of shapes.
- Create a Mini Game: Challenge yourself by creating a simple game using SVG shapes and animation. For example, you could create a game where the player controls a character (represented by a shape) to avoid obstacles (other shapes) moving across the screen.