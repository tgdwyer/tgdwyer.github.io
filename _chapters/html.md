---
layout: chapter
title: "HTML as a Declarative Language"
---

## Learning Outcomes

- Understand what makes HTML a declarative language and how it describes the structure and content of a web page.
- Comprehend the separation of concerns by differentiating structure (HTML), presentation (CSS), and custom behaviour (JavaScript).
- Understand how to include and define a basic SVG element within an HTML document
- Understand how an SVG element may be animated either declaratively through CSS or imperatively through JavaScript
- Compare the dangers of imperative programs with hidden side effects versus the transparency of declarative style programs

## What is HTML?

HTML, or HyperText Markup Language, is the standard markup language used to create and design web pages. It provides the structure and content of a web page by using a system of markup tags and attributes. HTML documents are interpreted by web browsers to render the content visually for users.

## Declarative Nature of HTML

HTML is considered a declarative language because it focuses on describing the structure and content of a web page without specifying how to achieve it. Instead of giving step-by-step instructions for rendering elements, HTML allows developers to declare the desired structure and let the browser handle the rendering process.

## Key Aspects of HTML’s Declarative Nature

1. Descriptive Tags: HTML tags are descriptive elements that define the purpose and meaning of content. For example, `<p>` tags indicate a paragraph, `<h1>` to `<h6>` tags denote headings of varying levels, `<ul>` and `<ol>` represent unordered and ordered lists respectively. These tags describe the content they enclose rather than instructing how it should be displayed.  Pairs of opening and closing HTML tags (e.g. `<h1>` defines the start of a heading, `</h1>` marks the end) define *elements* in a Document Object Model (DOM).  Elements can be nested within each other such that the DOM is a hierarchical (tree) structure.

2. Attribute-Based: HTML elements can have attributes that provide additional information or functionality. Attributes like class, id, src, href, etc., provide hooks for styling, scripting, or specifying behavior. However, these attributes do not dictate how elements are displayed; they simply provide metadata or instructions to browsers.

3. Separation of Concerns: Modern HTML5 is actually a suite of languages which encourages a separation of concerns by delineating structure (HTML), presentation (CSS), and behavior (JavaScript). This promotes maintainability and scalability by allowing each aspect of web development to be managed independently.

4. Browser Interpretation: HTML documents are interpreted by web browsers, which render the content based on the instructions provided in the markup. Browsers apply default styles and layout algorithms to HTML elements, ensuring consistency across different platforms and devices.

## An Animated Rectangle Using SVG in HTML

A [live version of the following code is available together with an online editor](https://stackblitz.com/edit/stackblitz-starters-6m6cfd?file=index.html) for you to experiment with.  Or you can create the files locally on your computer, all in the same directory, and drag-and-drop the `index.html` file into your browser to see the animation.
We are going to build an HTML page that looks something like this:
![Animated Rectangles Screenshot](/assets/images/chapterImages/html/animatedrects.gif)

### Step 1: Setting Up the HTML Document

First, create a new HTML file (name it `index.html`) and define the basic structure of the document:

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

In this step, we’ve set up the basic HTML structure with a `<!DOCTYPE>` declaration, `<html>`, `<head>`, and `<body>` tags. We’ve also included meta tags for character encoding and viewport settings, as well as a title for the page.

### Step 2: Adding an SVG Element

Next, let’s add an SVG element to the body of our HTML document. This SVG element will contain the rectangle that we’ll animate later:

```html
<body>
  <svg width="100" height="30">
    <!-- SVG content will go here -->
  </svg>
</body>
```

We’ve added an SVG element with a width and height of 100 units each. This provides a canvas for our SVG graphics.

### Step 3: Adding a Rectangle to the SVG

Now, let’s add a rectangle `<rect>` element inside the SVG to represent the moving rectangle:

```html
<svg width="100" height="30">
  <rect id="blueRectangle" x="10" y="5" width="20" height="20" fill="blue"/>
</svg>
```

In this step, we’ve defined a rectangle with a starting position at coordinates (10, 10) and a width and height of 20 units each. The rectangle is filled with a blue color.  Importantly, we’ve given the `<rect>` element a unique id “blueRectangle” by which we can refer to it elsewhere, below we’ll demonstrate adding an animation behaviour to this rectangle using this id from CSS or JavaScript.

Most HTML elements, including SVG elements have certain attributes according to their documentation, which determine how they are rendered and behave in the browser. [MDN](https://developer.mozilla.org/en-US/docs/Web/SVG/Element/rect) is normally a good reference for what is available to use.

### Step 4: Adding Animation declaratively using CSS

There are many ways to achieve the same thing in HTML. We will now look at how an animation may be added *declaratively* to our SVG rectangle using CSS.  First, we need to tell the browser where to find our CSS file from the (`<head>`) element of our HTML.

```html
<head>
...
  <link rel="stylesheet" href="style.css" >
</head>
```

Now we create the `style.css` file as follows:

```css
#blueRectangle {
  animation-name: moveX;
  animation-duration: 5s;
  animation-timing-function: linear;
  animation-fill-mode: forwards;
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

This first clause *selects* the rectangle by the unique id we gave it: `blueRectangle`, and then declares some style attributes for animation that specify:

- an id for the animation: `moveX`;
- a duration of 5 seconds; 
- that the animation should be interpolated linearly (as opposed to something non-linear like ease-in-out);
- and that it should play forwards as opposed to a gamut of other options.  
  
In the `keyframes` declaration we declare style properties which should be applied at different percentages of completion of the `moveX` animation. The browser will interpolate between them according to the other style settings we specified.  In this case, we have simply set an initial and final `x` position for the rectangle.

This is a program of sorts (in that it causes a lot of computation to happen in the browser with outputs that we can see on our webpage), but it’s declarative in the sense that we did not tell the browser *how* to perform the animation.  Rather we *declared* what we wanted the rectangle to look like at the start and end of the animation and let the browser figure out how to perform the transition.

### Alternate Step 4: Adding a custom animation from an imperative Javascript program

By contrast, we can create an *imperative* JavaScript program which explicitly gives the list of instructions for *how* to move the rectangle. 
We'll create another rectangle with the id "redRectangle" which we can manipulate from javascript by including a reference to a js file, e.g., `script.js`

```html
<body>
  ...
  <svg width="100" height="30" id="svg">
    <rect id="redRectangle" x="10" y="5" width="20" height="20" fill="red"/>
  </svg>
  <script src="script.js"></script>
</body>
```

We now create a function which encodes the precise steps to animate the rectangle at 60 FPS (the `setTimeout` call queues up each successive frame of animation). If you have experience with other languages like python hopefully this will be understandable even if the syntax looks a bit unfamiliar.  If it's not completely clear yet don't worry. We'll point out the things that are important to note for now below, but you can refer to our [intro to JavaScript](javascript1) for the basics.

```javascript
// Define an animation function
function animate(rect, startX, finalX, duration) {
    const 
      startTime = performance.now(),
      endTime = startTime + duration;
    function nextFrame() {
        // Calculate elapsed time
        const 
          currentTime = performance.now(),
          elapsedTime = currentTime - startTime;
              
        // Check if animation duration has elapsed
        if (elapsedTime >= duration) {
            // Set the final position of the rectangle.
            // We can use `setAttribute` to modify the HTML Element. In this case, we are changing the x attribute. 
            rect.setAttribute('x', finalX);
            return; // Stop the animation
        }

        // Calculate position based on elapsed time
        const x = startX + (finalX - startX) * elapsedTime / duration;

        // Set the intermediate position of the rectangle.
        rect.setAttribute('x', x);
    
        // Call the nextFrame function again after a delay of 1000/60 milliseconds
        setTimeout(nextFrame, 1000 / 60); // 60 FPS
    }
    nextFrame();
}
  
const rectangle = document.getElementById('redRectangle')
const duration = 5000; // 5 seconds in milliseconds
animate(rectangle, 0, 370, duration);
```

However, there are some serious issues with this code.

- Obviously it's more complex and requires more code than using the built-in CSS animation feature.
- The `animate` function updates the state of the DOM (the `x` position of the rectangle) from deep inside it's logic. Normally, we look for outputs of functions in the value that they `return`, but this function has no explicit return value.  To see what it does, we have to carefully inspect the code to identify the line which causes the *side effect* of moving the rectangle (the `rect.setAttribute` calls).  
- Another hidden complexity is the asynchronous (or delayed) behaviour due to the use of  `setTimeout` to queue up the successive frames of animation. Such hidden side effects and complexity are the opposite of the intention of declarative-style programming.

Later, we will see how [functional reactive programming](/functionalreactiveprogramming) techniques can be used to separate code with such side effects on global state from code that implements behavioural logic in interactive web pages.

## Conclusion

We saw that the philosophy of HTML programming is primarily *declarative* in the sense that the programmer/designer tells the browser what they want to see and rely on the browser’s sophisticated rendering engine to figure out how to display the content.  This extends to adding dynamic behaviours such as animation declaratively through CSS.

By contrast, we saw a different *imperative* approach to adding an animation to our web page using JavaScript. There are pros and cons to each:

<!-- markdownlint-disable MD036 -->

*Declarative*

- Easier and faster to code, easier to read
- Less likely to be buggy
- We spend less time on trying to figure out *how* to do things and focus on *what* we want

*Imperative*

- Greater control than the pure declarative HTML/CSS approach
- We can create richer interaction

A major theme of this course will be seeing how *pure functional programming* techniques give us a coding style that:

- is as powerful as imperative programming
- but is more declarative (through functions that clearly declare their inputs and outputs)
- and forces us to clearly distinquish code with side effects from pure computation.
<!-- markdownlint-enable MD036 -->

## Exercises

- Modify Rectangle Properties: Experiment with changing the attributes of the rectangle such as width, height, fill color, and position. See how these changes affect the appearance of the rectangle on the screen.
- Animate Different Shapes: Besides rectangles, try animating other SVG shapes such as circles, ellipses, or polygons. Explore how the animation function can be adapted to work with different types of shapes.
- Create a Mini Game: Challenge yourself by creating a simple game using SVG shapes and animation. For example, you could create a game where the player controls a character (represented by a shape) to avoid obstacles (other shapes) moving across the screen.

## Glossary

*HTML*: Hyper-Text Markup Language - the declarative language for specifying web page content.

*CSS*: Cascading Style Sheets - another declarative (part of the HTML5 standard) for specifying reusable styles for web page rendering.

*SVG*: Scalable Vector Graphics - another part of the HTML standard for specifying images declaratively as sets of shapes and paths.