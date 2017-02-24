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

{% plantuml %}
[First] - [Second]
{% endplantuml %}

{% github_sample_ref /anatoliykmetyuk/free-acp/blob/0932ccde36b0efa83dd01b25ca1fee393154d987/core/src/main/scala/freeacp/Tree.scala %}
{% highlight scala %}
{% github_sample /anatoliykmetyuk/free-acp/blob/0932ccde36b0efa83dd01b25ca1fee393154d987/core/src/main/scala/freeacp/Tree.scala 43 53 %}
{% endhighlight %}

{% xdot png %}  
    digraph finite_state_machine {
    rankdir=LR;
    size="8"
    node [shape = doublecircle]; LR_0 LR_3 LR_4 LR_8;
    node [shape = circle];
    LR_0 -> LR_2 [ label = "SS(B)" ];
    LR_0 -> LR_1 [ label = "SS(S)" ];
    LR_1 -> LR_3 [ label = "S($end)" ];
    LR_2 -> LR_6 [ label = "SS(b)" ];
    LR_2 -> LR_5 [ label = "SS(a)" ];
    LR_2 -> LR_4 [ label = "S(A)" ];
    LR_5 -> LR_7 [ label = "S(b)" ];
    LR_5 -> LR_5 [ label = "S(a)" ];
    LR_6 -> LR_6 [ label = "S(b)" ];
    LR_6 -> LR_5 [ label = "S(a)" ];
    LR_7 -> LR_8 [ label = "S(b)" ];
    LR_7 -> LR_5 [ label = "S(a)" ];
    LR_8 -> LR_6 [ label = "S(b)" ];
    LR_8 -> LR_5 [ label = "S(a)" ];
    overlap=false;
    fontsize=10;
}
{% endxdot %}
{% xneato svg %}    
digraph TrafficLights {
node [shape=box];  gy2; yr2; rg2; gy1; yr1; rg1;
node [shape=circle,fixedsize=true,width=0.9];  green2; yellow2; red2; safe2; safe1; green1; yellow1; red1;
gy2->yellow2;
rg2->green2;
yr2->safe1;
yr2->red2;
safe2->rg2;
green2->gy2;
yellow2->yr2;
red2->rg2;
gy1->yellow1;
rg1->green1;
yr1->safe2;
yr1->red1;
safe1->rg1;
green1->gy1;
yellow1->yr1;
red1->rg1;

overlap=false
label="PetriNet Model TrafficLights\nExtracted from ConceptBase and layed out by Graphviz"
fontsize=12;
}
{% endxneato %}