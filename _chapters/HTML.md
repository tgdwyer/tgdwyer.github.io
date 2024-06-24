
# HTML as a Declarative Language

## What is HTML?

HTML, or HyperText Markup Language, is the standard markup language used to create and design web pages. It provides the structure and content of a web page by using a system of markup tags and attributes. HTML documents are interpreted by web browsers to render the content visually for users.

## Declarative Nature of HTML

HTML is considered a declarative language because it focuses on describing the structure and content of a web page without specifying how to achieve it. Instead of giving step-by-step instructions for rendering elements, HTML allows developers to declare the desired structure and let the browser handle the rendering process.

## Key Aspects of HTML's Declarative Nature:

1. Descriptive Tags: HTML tags are descriptive elements that define the purpose and meaning of content. For example, \<p\> tags indicate a paragraph, \<h1\> to \<h6\> tags denote headings of varying levels, \<ul\> and \<ol\> represent unordered and ordered lists respectively. These tags describe the content they enclose rather than instructing how it should be displayed.

2. Attribute-Based: HTML elements can have attributes that provide additional information or functionality. Attributes like class, id, src, href, etc., provide hooks for styling, scripting, or specifying behavior. However, these attributes do not dictate how elements are displayed; they simply provide metadata or instructions to browsers.

3. Separation of Concerns: HTML encourages a separation of concerns by delineating structure (HTML), presentation (CSS), and behavior (JavaScript). This promotes maintainability and scalability by allowing each aspect of web development to be managed independently.

4. Browser Interpretation: HTML documents are interpreted by web browsers, which render the content based on the instructions provided in the markup. Browsers apply default styles and layout algorithms to HTML elements, ensuring consistency across different platforms and devices.

## An Animated Rectangle Using SVG in HTML

### Step 1: Setting Up the HTML Document
First, create a new HTML file and define the basic structure of the document:

```html
Copy code
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

In this step, we've set up the basic HTML structure with a \<!DOCTYPE\> declaration, \<html\>, \<head\>, and \<body\> tags. We've also included meta tags for character encoding and viewport settings, as well as a title for the page.

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
Now, let's add a rectangle (<rect>) element inside the SVG to represent the moving rectangle:

```html
<svg width="100" height="100">
  <rect x="10" y="10" width="20" height="20" fill="blue"/>
</svg>
```

In this step, we've defined a rectangle with a starting position at coordinates (10, 10) and a width and height of 20 units each. The rectangle is filled with a blue color. 

Most HTML elements, including SVG elements have certain attributes according to their documentation, which determine how they are rendered and behave in the browser. [MDN](https://developer.mozilla.org/en-US/docs/Web/SVG/Element/rect) is normally a good reference for what is available to use.

### Step 5: Adding Some Javascript

We can integrate some javascript, by including a reference to a file, e.g., `script.js`

```html
<body>
  <svg width="100" height="100" id="svg">
    <rect x="10" y="10" width="20" height="20" fill="blue" id="movingRect"/>
  </svg>
  <script src="script.js"></script>
</body>
```

Please note the addition of the `id` tag, this is **important** and it will be the name used in Javascript to refer to this rectangle. 

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

const rectangle = document.getElementById('movingRect')
// Start the animation
animate(rectangle, 10, 50, null);
```

---

## Exercises

- Modify Rectangle Properties: Experiment with changing the attributes of the rectangle such as width, height, fill color, and position. See how these changes affect the appearance of the rectangle on the screen.
- Animate Different Shapes: Besides rectangles, try animating other SVG shapes such as circles, ellipses, or polygons. Explore how the animation function can be adapted to work with different types of shapes.
- Create a Mini Game: Challenge yourself by creating a simple game using SVG shapes and animation. For example, you could create a game where the player controls a character (represented by a shape) to avoid obstacles (other shapes) moving across the screen.

---
