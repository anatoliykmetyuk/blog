---
layout: post
title: Test Plugins
categories:
- blog
description: Dummy test page
keywords: [test]
---
This is the third part of my progress report on a rewriting-based implementation of [SubScript](https://github.com/scala-subscript/subscript), [FreeACP](https://github.com/anatoliykmetyuk/free-acp). This part covers the architecture of FreeACP I came up with so far while implementing the rewriting engine for SubScript.

If you have not read the previous parts of this report, you are advised to do so before reading this one:

The most interesting part of the engine is the resumption:

$\frac{-b\pm\sqrt{b^2-4ac}}{2a}$

```{.scala include=../code/matryoshka-intro/src/main/scala/matryoshkaintro/Main.scala snippet=cool-thingy}
```

```scala
def foo(x: Int) = bar
```
