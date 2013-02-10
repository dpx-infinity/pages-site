---
title: Builder pattern in Java using static methods
date: 30/09/2012
tags: programming, java, patterns
---

As you possibly know, Java as a language is not very expressive. However, using its some not very known features it
is possible to implement some useful patterns, one of them being _builder_.

Java users usually understand the following construction under Builder.

```java
SomeComplexObject object =
    new SomeComplexObjectBuilder()
        .setParameter1(x)
        .setParameter2(y)
        .setParameterN(z)
        .build();
```

However, the builder pattern I'm writing about is more known in Groovy world. It is very easy to implement and use it 
there. It looks like this (I may be somewhat wrong in Groovy syntax, but this is just to illustrate general idea):

```groovy
XML xml = buildXML {
    elem "persons" {
        elem "person" {
            attr "name" "Ivan"
            attr "enabled" true
        }
        elem "person" {
            attr "name" "Petr"
            attr "enabled" false
        }
    }
}
```

It is possible to create something like this in Java. It looks like this:

```java
XML xml = buildXML(
    elem("persons",
        elem("person",
            attr("name", "Ivan"),
            attr("enabled", true)
        ),
        elem("person",
            attr("name", "Petr"),
            attr("enabled", false)
        )
    )
);
```

Let's make it. Internal machinery behind this construction is fairly simple. First we define data class, i.e. `XML`:

```java
package org.example.xmlbuilder;

import java.util.Map;
import java.util.HashMap;
import java.util.List;
import java.util.ArrayList;

public class XML {
    public final String name;
    public final Map<String, String> attributes = new HashMap<String, String>();
    public final List<XML> children = new ArrayList<XML>();

    public XML(String name) {
        this.name = name;
    }
}
```

We're using public final fields here (not standard Java accessors) because the example is very simple,
and it is redundant to create getters for final fields (and for setters it is also [almost] impossible).

Now we create a class which will perform the actual work. It looks like this:

```java
package org.example.xmlbuilder;

public final class XMLBuilder {
    private XMLBuilder() {
    }

    public static final class Elem {
        private final XML xml;

        private Elem(XML xml) {
            this.xml = xml;
        }
    }

    public static final class Attr {
        private final String name;
        private final String value;

        private Attr(String name, String value) {
            this.name = name;
            this.value = value;
        }
    }

    public static XML buildXML(Elem root) {
        return root.xml;
    }

    public static Elem elem(String name, Object... objs) {
        XML xml = new XML(name);
        for (Object obj : objs) {
            if (obj instanceof Elem) {
                xml.children.add(((Elem) obj).xml);
            } else if (obj instanceof Attr) {
                Attr au = (Attr) obj;
                xml.attributes.put(au.name, au.value);
            } else {
                throw new IllegalArgumentException("Invalid object supplied: " + obj);
            }
        }
        return new Elem(xml);
    }

    public static Attr attr(String name, Object value) {
        return new Attr(name, value.toString());
    }
}
```

That's it. There really are nothing to explain, the code is very simple. There are three static methods,
`buildXML`, `elem` and `attr`. `elem` and `attr` return intermediate objects (`Elem` and `Attr` inner classes) which
contain parts of the whole structure, and `buildXML` returns an actual `XML` object from the top-level `Elem`.
Maybe only `elem` body requires attention. It accepts variable number of arguments of different types. You can provide
it `Elem`s, which will become child objects, and `Attr`s, which will set attribute values.

Now, to use it just like in our example above, we have to `import static` members of `XMLBuilder` class:

```java
import static org.example.xmlbuilder.XMLBuilder.buildXML;
import static org.example.xmlbuilder.XMLBuilder.elem;
import static org.example.xmlbuilder.XMLBuilder.attr;

...
```

or, simpler:

```java
import static org.example.xmlbuilder.XMLBuilder.*;

...
```

Now it is possible to use `XMLBuilder`'s methods just like if they were static methods in current class (or even
global functions, using C++ terms).

I'm absolutely sure that I'm not the first who came up with this idea, but I couldn't find implementations of such or
similar patterns in open source libraries or programs. It is possible that this is so because of performance
reasons. Obviously, building an object in this way involves construction of several other objects, and it is
certainly quickier to simply invoke constructor.

However, there are not that many extra objects are created, and since they are short-lived, they will be quickly
taken out by Java generational garbage collector. I think it is perfectly reasonable to use this approach if you
want to get readable code.
