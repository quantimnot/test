# Tests

There are various methods and idiologies for testing code, or code under test (CUT).

## Types

### Unit

Tests a small, indivisible, unit of functionality.
Includes both functional and non-functional tests.

#### Design By Contract

Use assertions to test pre and post conditions of a unit.

#### Inline Vs Isolated

### Integration

Tests one or units that integrate with each other.
Includes both functional and non-functional tests.

### System

System = {Functional, Non-functional}

### Functional

Test whether required functionality of a unit, integration or system is met.

### Non-Functional

Test whether non-functional aspects of a unit, integration or system.

#### Performance

##### Benchmarking

Performance is tested by benchmarking a metric against previous revisions or other implementations.

##### Profiling

Sample the performance metrics of the divisible units or integrations to find where bottlenecks are present.

#### Reliability

Test the reliablity of a unit, integration or system.

Methods of reliability testing include fuzzing and fault injection.

Test outcomes are to see how well unexpected inputs or environmental conditions are handled.

#### Security

Can involve both static analysis of the software and runtime analysis to find security weakness.

Fuzzing can be used to test unhandled inputs that can be exploited.

## Excercise Method

There are various methods that are used to test the

### Direct Code Invocation

This is where test software is compiled/interpreted to directly invoke the software code.

### Terminal UI Driver

An external program excercises the software by:
- writing to its stdin
- reading from its stdout
- reading from its stderr
- sending OS signals

### Native Graphical UI Driver

An external program excercises the software by:
- writing to its stdin
- reading from its stdout
- reading from its stderr
- sending OS signals
- sending keyboard events
- sending mouse events
- sending touch events
- comparing rendered sections of the screen

### HTML UI Driver

An external program excercises the software by:
- sending keyboard events
- sending mouse events
- sending touch events
- comparing DOM elements
- comparing rendered sections of the screen

## Metrics

### Resource Usage

#### Time

What is the user and cpu duration?

#### Memory

What is the memory usage profile?

#### Storage

What is the disk usage profile?

#### Network

What is the network usage profile?


## Continuous Integration


## Current State of Nim Testing

The Nim compiler and standard library are tested using