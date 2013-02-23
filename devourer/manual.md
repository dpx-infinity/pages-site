---
title: Devourer overview
date: 2013-02-23
---

Devourer is a library for streaming XML processing. It is a wrapper around StAX parsing library
providing easy-to-use mechanism to configure actions which should be executed on certain nodes
inside the XML document. These actions can do anything with the data they receive, e.g. construct
new objects. To hold intermediate state stack data structure is used.

Devourer is very simple to use. First you create and configure an instance of `Devourer` class and
then you feed it an XML document. Devourer accepts `InputStream`s, `Reader`s, byte arrays and
strings. After parsing it produces `Stacks` object which contains results of the processing.

Inspirations
------------

Devourer is heavily influenced by [Apache Commons Digester](http://commons.apache.org/digester/). In
fact, it does the same work as Digester, but there are some differences.

First, Devourer is based on StAX library instead of SAX. This leads to much cleaner and easier to
support parsing code, as well as nearly effortless thread safety. StAX provides so-called 'pull'
parser, as opposed to SAX 'push' parser. The difference is that in SAX *the parser* invokes
callbacks provided by you when it reaches certain XML entities, while with the StAX parser *you*
advance the parser manually using iterator-like syntax. StAX parser allows writing much cleaner code
because it gives more control over parsing process. Since you call its `next()` method manually, it
is possible to run all XML processing inside a single method call, thus removing the need to hold
intermediate state inside the object fields. This works very nicely with concurrency.

In fact, Devourer provides an SAX-like API in the sense that it relies on callbacks too; however,
Devourer's callbacks are intended to be single-purpose and much lighter; they also do not have
internal state and instead work with state container provided by Devourer.

Second, Devourer does not use reflection (almost, and even that is optional); it relies on callbacks
defined as anonymous classes implementing interfaces.  Not only this removes performance drop of
reflection; the actions interfaces are designed to integrate nicely with Java 8 Lambda extensions.
Actions interfaces are *functional interfaces* in term of Java 8; this means that anywhere they are
expected *lambda expressions* can be used.  Because of this it is possible to define actions using
very clean, terse and understandable syntax, while not sacrificing performance. Reflection is used
in another variant of configuration based on annotations.  It is optional, but it still can be
useful because of its readability.

Third, Devourer has minimum amount of runtime dependencies. It only depend on
[Google Guava](http://code.google.com/p/guava-libraries/) library, which it uses rather
heavily. Guava provides various utilities including immutable collections, I/O helpers,
preconditions, Optional etc. Guava encourages modern programming style including usage of immutable
collections and null-hostility. Devourer is created on those principles, and I believe they make the
code much easier to write and support.

I see above points as advantages of Devourer over Digester. However, Digester still may be
preferrable for you. Digester is much more mature project. Its configuration may be somewhat easier
to understand than Devourer annotation configuration and certainly is easier than Devourer modular
configuration (at least on Java < 8 and when not using IDE folding). Its reflection-based rules
integrate nicely with JavaBean convention. Digester supports loading rules from XML files, which
allows dynamic reconfiguration of the parsing process (Devourer may support something like this one
day). Digester (at least v3) supports plugins, which are used to dynamically reconfigure digesting
rules during the processing. Digester support for XML namespaces is much better than Devourer's
(this is something I will try to fix in the nearest future).

Configuration
-------------

Configuration of Devourer is done using an object of certain kind. Configuration object is a mapping
from paths inside XML document to series of actions which should be executed on the nodes which are
located at the specified path. There are two variants of configuration objects, equivalent in power
but different in usage: *modules* and *annotated objects*.

Modular configuration looks like [Google Guice](http://code.google.com/p/google-guice/) or Digester
modules. It provides very lightweight DSL to help you define actions which should be taken on
different nodes of the document. It is designed in such way that it is compatible with Java 8 Lambda
extension; moreover, modern IDEs (e.g. IntelliJ IDEA) can fold anonymous classes into something
highly resembling lambdas, so configuration modules look fairly lean and straightforward, without
sacrificing effectiveness of direct code invocation (opposite to reflective calls of Digester). This
is the preferred method of configuration.

Devourer also supports annotated configuration, which is heavily inspired by Spring Web MVC
controllers. A class can be annotated in such way that its methods will be regarded as actions. This
approach to configuration does require some reflection to be used during parsing process, so the
modular approach should be preferred if you need as much performance as possible; however, annotated
configuration is slightly more readable and understandable.

During the processing actions modify `Stacks` object, which essentially is a set of named
stacks. This object is used to hold intermediate state of the processing (e.g. builders or
unfinished data structures); it is also returned by Devourer, so it should hold processing results,
e.g. finished object. `Stacks` object is not reused in different parsing executions, so holding
state in it is completely thread-safe, consequently, the same Devourer object can be used from
multiple threads simultaneously without any concurrency problems.

Example
-------

Here is a quick example. Suppose you have the following XML document:

```xml
<persons>
  <person id="1">
    <name>Foo Bar</name>
    <logins>
      <login site="example.com">foobar</login>
      <login site="example.org">f.bar</login>
    </logins>
  </person>
  <person id="2">
    <name>Baz Boo</name>
    <logins>
      <login site="uni.edu">boo.baz</login>
    </logins>
  </person>
  <person id="4">
    <name>Fizz B. Jr</name>
  </person>
</persons>
```

You want to parse it into the list of objects of the following class:

```java
public class Person {
    public final int id;
    public final String name;
    public final List<Login> logins;

    public Person(int id, String name, List<Login> logins) {
        this.id = id;
        this.name = name;
        this.logins = logins;
    }
}

public class Login {
    public final String site;
    public final String value;

    public Login(String site, String value) {
        this.site = site;
        this.value = value;
    }
}
```

Then the following actions mapping will do the work for you:

```java
public class PersonModule extends AbstractMappingModule {
    @Override
    public void configure() {
        on("/persons")
            .doBefore(new ReactionBefore() {
                @Override
                public void react(Stacks stacks, AttributesContext context) {
                    stacks.push(ImmutableList.builder());
                }
            })
            .doAfter(new ReactionAfter() {
                @Override
                public void react(Stacks stacks, AttributesContext context) {
                    ImmutableList.Builder<Person> builder = stacks.pop();
                    stacks.push(builder.build());
                }
            });

        on("/persons/person")
            .doBefore(new ReactionBefore() {
                @Override
                public void react(Stacks stacks, AttributesContext context) {
                    stacks.push("person", context.attribute("id").get());
                }
            })
            .doAfter(new ReactionAfter() {
                @Override
                public void react(Stacks stacks, AttributesContext context) {
                    String name = stacks.pop("person");
                    int id = Integer.parseInt(stacks.<String>pop("person"));
                    ImmutableList.Builder<Login> loginsBuilder = stacks.pop("logins");
                    ImmutableList.Builder<Person> personsBuilder = stacks.peek();
                    personsBuilder.add(new Person(id, name, loginsBuilder.build()));
                }
            });

        on("/persons/person/name")
            .doAt(new ReactionAt() {
                @Override
                public void react(Stacks stacks, AttributesContext context, String body) {
                    stacks.push("person", body);
                }
            });

        on("/persons/person/logins")
            .doBefore(new ReactionBefore() {
                @Override
                public void react(Stacks stacks, AttributesContext context) {
                    stacks.push("logins", ImmutableList.builder());
                }
            })
            .doAfter(new ReactionAfter() {
                @Override
                public void react(Stacks stacks, AttributesContext context) {
                    stacks.push(
                        "logins", 
                        stacks.<ImmutableList.Builder<Login>>pop("logins").build()
                    );
                }
            });

        on("/persons/person/logins/login")
            .doAt(new ReactionAt() {
                @Override
                public void react(Stacks stacks, AttributesContext context, String body) {
                    stacks.<ImmutableList.Builder<Login>>peek("logins")
                          .add(new Login(context.attribute("site").get(), body));
                }
            });
    }
}
```

This looks awfully verbose; however, it would look much better from inside the IDE, where code
folding hides most of the verboseness. Furthermore, with Java 8 there will be no verboseness at all!
This is how the module would look in Java 8:

```java
public class PersonModule extends AbstractMappingModule {
    @Override
    protected void configure() {
        on("/persons")
            .doBefore((stacks, context) -> stacks.push(ImmutableList.builder()))
            .doAfter((stacks, context) -> {
                ImmutableList.Builder<Person> builder = stacks.pop();
                stacks.push(builder.build());
            });

        on("/persons/person")
            .doBefore((stacks, context) -> stacks.push("person", context.attribute("id").get()))
            .doAfter((stacks, context) -> {
                String name = stacks.pop("person");
                int id = Integer.parseInt(stacks.<String>pop("person"));
                ImmutableList.Builder<Login> loginsBuilder = stacks.pop("logins");
                ImmutableList.Builder<Person> personsBuilder = stacks.peek();
                personsBuilder.add(new Person(id, name, loginsBuilder.build()));
            });

        on("/persons/person/name")
            .doAt((stacks, context, body) -> stacks.push("person", body));

        on("/persons/person/logins")
            .doBefore((stacks, context) -> stacks.push("logins", ImmutableList.builder()))
            .doAfter((stacks, context) -> {
                stacks.push(
                    "logins",
                    stacks.<ImmutableList.Builder<Login>>pop("logins").build()
                );
            });

        on("/persons/person/logins/login")
            .doAt((stacks, context, body) -> {
                stacks.<ImmutableList.Builder<Login>>peek("logins")
                      .add(new Login(context.attribute("site").get(), body));
            });
    }
}
```

In fact, IntelliJ IDEA folds code in such way that you see it very similar to this even with Java
6/7 language level.

Module definition was the hardest part. Actual XML parsing is very simple:

```java
Devourer devourer = Devourer.create(new PersonModule());
Reader reader = obtainReaderForXMLSomewhere();
Stacks stack = devourer.parse(reader);
List<Person> persons = stacks.pop();
```

That's it. `persons` will be a list of `Person` objects read from the XML and constructed via actions
configured in the module.
