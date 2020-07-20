---
layout: page
title: "Functional Reactive Programming"
permalink: /functionalreactiveprogramming/
---


## Learning Outcomes


## Introduction

Modern computer systems often have to deal with asynchronous processing.  Examples abound:
 - In RESTful web services, where a client sends a non-blocking request (e.g. GET) with no guarantee of when the server will send a response.
 - In user interfaces, events are triggered by user interaction with different parts of the interface, which may happen at any time.
 - Robotics and other systems with sensors, the system must respond to events in the world.

Under the hood, most of these systems work on an event model, a kind of single-threaded multitasking where the program (after initialisation) polls a FIFO queue for incoming events in the so-called event loop.  When an event is popped from the queue, any subscribed actions for the event will be applied.

In JavaScript the first event loop you are likely to encounter is the browser’s.  Every object in the DOM (Document Object Model - the tree data structure behind every webpage) has events that can be subscribed to, by passing in a callback function which implements the desired action.  We saw a basic click handler earlier.

Handling a single event in such a way is pretty straightforward.  Difficulties arise when events have to be nested to handle a (potentially-bifurcating) sequence of possible events.

A simple example that begins to show the problem is implementing a UI to allow a user to drag an object on (e.g.) and SVG canvas.  The state machine that models this is pretty simple:
