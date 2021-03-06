---
title: Devourer overview
date: 27/02/2013
version: 2
devourerVersion: 0.1
---

This document is an official manual for **Devourer XML processing library**. This page will be
updated with the changes in new releases of the library.

Devourer is a library for streaming XML processing. It is a wrapper around StAX parsing library
providing easy-to-use mechanism to configure actions which should be executed on certain elements
inside an XML document. These actions can do anything with the data they receive, e.g. construct new
objects. To hold intermediate state stack data structure is used.

Devourer is very simple to use. First you create and configure an instance of `Devourer` class and
then you feed it an XML document. Devourer accepts `InputStream`s, `Reader`s, byte arrays and
strings. After parsing it produces `Stacks` object which contains results of the processing.

Devourer's source code can be found at Bitbucket:
<https://bitbucket.org/googolplex/devourer>. Binary release is available at Maven Central:

```xml
<dependency>
    <groupId>org.bitbucket.googolplex.devourer</groupId>
    <artifactId>devourer</artifactId>
    <version>0.1</version>
</dependency>
```

Devourer is released under Apache 2.0 license, so you can use it everywhere you want.

Inspirations
------------

Devourer is heavily influenced by [Apache Commons Digester](http://commons.apache.org/digester/). In
fact, it does the same work as Digester, but there are some differences.

First, Devourer is based on StAX library instead of SAX. This leads to much cleaner and easier to
support parsing code, as well as nearly effortless thread safety. StAX provides so-called 'pull'
parser, as opposed to SAX 'push' parser. The difference is that in SAX **the parser** invokes
callbacks provided by you when it reaches certain XML entities, while with the StAX parser **you**
advance the parser manually using iterator-like syntax. StAX parser allows writing much cleaner code
because it gives more control over parsing process. Since you call its `next()` method manually, it
is possible to run all XML processing inside a single method call, thus removing the need to hold
intermediate state inside object fields. This works very nicely with concurrency.

In fact, Devourer provides a SAX-like API in the sense that it relies on callbacks too; however,
Devourer's callbacks are intended to be single-purpose and much lighter; they also do not have
internal state and instead work with state container provided by Devourer.

Second, Devourer does not use reflection (well, almost, but it is optional), it relies on callbacks
defined as anonymous classes implementing certain interfaces instead. Not only this removes
performance hit of reflection; the action interfaces are designed to integrate nicely with Java 8
Lambda extensions. Action interfaces are **functional interfaces** in term of Java 8; this means
that **lambda expressions** can be used anywhere where they are expected. Because of this it is
possible to define actions using very clean, terse and understandable syntax, while not sacrificing
performance. Reflection is used in another variant of configuration based on annotations. It is
optional, but it still can be useful because of its readability.

Third, Devourer has minimum amount of runtime dependencies. It only depend on
[Google Guava](http://code.google.com/p/guava-libraries/) library, which it uses rather
heavily. Guava provides various utilities including immutable collections, I/O helpers,
preconditions, Optional etc. Guava encourages modern programming style including usage of immutable
collections and null-hostility. Devourer is created using these principles, and I believe they make
the code much easier to write and support.

I see above points as advantages of Devourer over Digester. However, Digester still may be
preferrable for you. Digester is much more mature project. Its configuration may be somewhat easier
to understand than Devourer annotation configuration and certainly is easier than Devourer modular
configuration (at least on Java < 8 and when not using IDE folding). Its reflection-based rules
integrate nicely with JavaBean convention. Digester supports loading rules from XML files, which
allows dynamic reconfiguration of the parsing process (Devourer may support something like this one
day). Digester (at least v3) supports plugins, which are used to dynamically reconfigure digesting
rules during the processing. Digester support for XML namespaces is much better than Devourer's
(this is something I will try to fix in the nearest future). Digester's API is stable; Devourer's
API will evolve, at least during first several versions.

Devourer workflow overview, basic concepts
------------------------------------------

### Paths

As I said, Devourer is a tool for XML processing. Essentially it is a wrapper over streaming XML
parser which can be configured to execute arbitrary code (each piece of which is called **action**)
on elements of an XML document. This code usually constructs some Java object, though it is not
required. Actions are set to be executed on elements using these elements' **path** inside the
document tree. A path which Devourer uses is similar to XPath expression, though much simpler. It
looks like conventional path inside Unix filesystem: `/path/to/node`. For example, in the following
document:

```xml
<root>
  <subtree>
    <node>
      <name>Abcd</name>
    </node>
  </subtree>
</root>
```

the most inner element `<name>` will be located at `/root/subtree/node/name` path.

The path should be declared without XML namespace prefixes. Devourer will not handle qualified names
in the path correctly. Namespaces support is somewhat lacking in the Devourer currently; I intend to
fix this in the nearest versions.

The path can match multiple nodes in the document, like in the following piece:

```xml
<root>
  <subtree>
    <node>foo bar</node>
  </subtree>
  <subtree>
    <node>baz boo</node>
    <node>woo zoo</node>
  </subtree>
</root>
```

There are exactly three elements which match `/root/subtree/node` path and two elements which match
`/root/subtree` path. If some actions are configured on these paths, they will be executed one time
for each one of the corresponding elements.

### Stacks

To assist actions in holding their state, for example, parts of the object being constructed,
Devourer provides `Stacks` object. `Stacks` is a collection of stacks (which is not hard to
infer). `Stacks` interface provides an access to a number of stacks using their names. It also
provides direct access to some default stack. This is simply a shortcut to access default stack by
its name, `"main"`. Stacks are created on the first access to them, so it is possible to have
arbitrary amount of them.

Actions should use `Stacks` to store their intermediate state. The single `Stacks` instance is not
reused by Devourer in separate parsing processes, which means that if you hold all the state you
need inside `Stacks`, your actions will be completely thread-safe.

Devourer returns `Stacks` object as a result of the parsing process, so if you need to return some
object from the actions to your application code, use `Stacks`. Note, however, that default
implementation of `Stacks` interface is not thread-safe by itself. This is not a problem for the
Devourer since it never uses single instance of `Stacks` from multiple threads, but because it is
returned from the `parse()` method to your code, you should know that using it from multiple threads
is a bad idea.

### Action types

XML has fairly complex structure when you look into its details, but Devourer tries to abstract all
these details into three types of events you can encounter. These types of events correspond to
three types of actions Devourer support.

These types define when the given action will be executed in regard to the XML element. These types
are **before-actions**, **at-actions** and **after-actions**. This scheme mirrors the one of
Digester.

*Before-actions* are executed when the start tag of the element is encountered. In Digester
terminology such actions are implemented using `begin()` method of `Rule` class. These actions are
suitable for the preparation of objects builders or objects themselves; it is also reasonable to
check element attributes here.

*At-actions* are executed when the textual content of the element is encountered. In Digester
terminology such actions are implemented using `body()` method of `Rule` class. These actions are
needed to retrieve text from the XML elements. Note that because of the way streaming parsers work
these actions could be invoked multiple times for the same element if this element's body is a mix
of text nodes and other elements, like in the following example:

```xml
<root>
  some text
  <inner>
    inner text
  </inner>
  another text
</root>
```

In case at-action has been configured on `/root` element, it will be invoked twice, first for `some
text` piece, second for `another text` piece.

It also should be noted that at-actions will not be invoked if the content consists entirely of
whitespace. It is also possible to tweak leading and trailing whitespaces handling by the means of
Devourer configuration.

*After-actions* are executed when the end tag of the element is encountered. In Digester terminology
such actions are implemented using `end()` element of `Rule` class. These actions are used to
construct final version of the objects.

All actions have access to `Stacks` object and `AttributeContext` object. `Stacks` object is
described above. `AttributeContext` (the name may be changed in future versions) is an object which
provides an access to current element parameters: element name and namespace and element
attributes. Usually you use `AttributeContext` to read element attributes.

At-actions also are provided with element content represented as `String`.

Mapping configuration
---------------------

Configuration of Devourer is done using an object of certain kind. Configuration object defines a
mapping from paths inside XML document to series of actions which should be executed on the nodes
which are located at the specified path. There are two variants of configuration objects, equivalent
in power but different in usage: *modules* and *annotated objects*.

### Modular configuration

Modular configuration looks like [Google Guice](http://code.google.com/p/google-guice/) or Digester
modules. It provides very lightweight DSL to help you define actions which should be taken on
different nodes of the document. It is designed in such way that it is compatible with Java 8 Lambda
extension; moreover, modern IDEs (e.g. IntelliJ IDEA) can fold anonymous classes into something
highly resembling lambdas, so configuration modules look fairly lean and straightforward, without
sacrificing effectiveness of direct code invocation (opposite to reflective calls of Digester). This
is the preferred method of configuration.

Modular configuration is usually defined by extending `AbstractMappingModule` class, though nothing
prevents you from implementing `MappingModule` interface directly. `AbstractMappingModule` has
single method, `configure()`, which is invoked by Devourer to configure mappings. It looks like
this:

```java
public class ExampleModule extends AbstractMappingModule {
    @Override
    public void configure() {
        on(<path>)
            .doBefore(new ActionBefore() {
                @Override
                public void act(Stacks stacks, AttributesContext context) {
                    <before-action>
                }
            })
            .doAt(new ActionAt() {
                @Override
                public void act(Stacks stacks, AttributesContext context, String body) {
                    <at-action>
                }                    
            })
            .doAfter(new ActionAfter() {
                @Override
                public void act(Stacks stacks, AttributesContext context) {
                    <after-action>
                }
             });

        on(<another-path>)
            .doBefore(...)
            .doAt(...)
            .doAfter(...);

        ...
    }
}
```

You can see that the body of `configure()` method consists of several `on()` method invocations
followed `doBefore()`, `doAt()` and `doAfter()` calls. These calls set up provided actions of
corresponding type to be invoked on corresponding elements specified by paths in XML document.

It should be noted that the order between different types of actions does not matter, that is,
before-actions can safely go after after-actions (no pun intended).However, *inside* the group of
actions of the same type defined on the same node the order *does* matter. It is guaranteed that
such actions will be invoked *exactly in definition order* during the parsing process.

It is also possible to add actions on the same path using multiple `on()` clauses. This does not
have any special effects, and the configuration behave exactly as if these actions were defined in
the same `on()` clause.

### Annotated configuration

Devourer also supports annotated configuration, which is heavily inspired by Spring Web MVC
controllers. A class can be annotated in such way that its methods will be regarded as actions. This
approach to configuration does require some reflection to be used during parsing process, so the
modular approach should be preferred if you need as much performance as possible; however, annotated
configuration is slightly more readable and understandable.

An object of any class can be regarded as annotated configuration. However, the class of this object
should be properly annotated for the configuration to work correctly.

Annotated configuration class consists only of methods. These methods must be annotated at least
with one of the following annotations: `@Before`, `@At`, `@After`. These annotations take single
string argument which must be correct path inside the XML document. These annotations define the
type of the action this method represents.

If method has return type other than `void`, it is assumed that the result of this method should be
pushed on the stack. `@PushTo` method annotation allows you to specify stack name explicitly. If
`@PushTo` is not specified, default stack will be used.

Annotated methods can have arbitrary number of parameters. During execution they will be injected
with different objects depending on their types and annotations. The following table describes all
possible variants of parameter types and annotation. In the table `T` means arbitrary type except
the ones listed in the first part of the table.

+-----------------------------------------+-------------------------------------------------------+
| Parameter type (possibly w/ annotation) | Description of the injected value                     |
+=========================================+=======================================================+
| `Stacks`                                | Current stacks object                                 |
+-----------------------------------------+-------------------------------------------------------+
| `AttributeContext` / `ElementContext`   | Current element context                               |
+-----------------------------------------+-------------------------------------------------------+
| `String` (only in `@At` methods and not | Element textual content                               |
| annotated with annotations described    |                                                       |
| below)                                  |                                                       |
+-----------------------------------------+-------------------------------------------------------+
| `@Pop T`                                | An object from the top of the default stack; the      |
|                                         | object is removed from the stack; an exception is     |
|                                         | thrown if the stack is empty                          |
+-----------------------------------------+-------------------------------------------------------+
| `@PopFrom(stackName) T`                 | An object from the top of the specified stack; the    |
|                                         | object is removed from the stack; an exception is     |
|                                         | thrown if the stack is empty                          |
+-----------------------------------------+-------------------------------------------------------+
| `@Peek T`                               | An object from the top of the default stack; the      |
|                                         | object is kept on the stack; an exception is thrown   |
|                                         | if the stack is empty                                 |
+-----------------------------------------+-------------------------------------------------------+
| `@PeekFrom(stackName) T`                | An object from the top of the specified stack; the    |
|                                         | object is kept on the stack; an exception is thrown   |
|                                         | if the stack is empty                                 |
+-----------------------------------------+-------------------------------------------------------+
| `@Pop Optional<T>`                      | An object from the top of the default stack; the      |
|                                         | object is removed from the stack; absent value is     |
|                                         | injected if the stack is empty                        |
+-----------------------------------------+-------------------------------------------------------+
| `@PopFrom(stackName) Optional<T>`       | An object from the top of the specified stack; the    |
|                                         | object is removed from the stack; absent value is     |
|                                         | injected if the stack is empty                        |
+-----------------------------------------+-------------------------------------------------------+
| `@Peek Optional<T>`                     | An object from the top of the default stack; the      |
|                                         | object is kept on the stack; absent value is injected |
|                                         | if the stack is empty                                 |
+-----------------------------------------+-------------------------------------------------------+
| `@PeekFrom(stackName) Optional<T>`      | An object from the top of the specified stack; the    |
|                                         | object is kept on the stack; absent value is injected |
|                                         | if the stack is empty                                 |
+-----------------------------------------+-------------------------------------------------------+

Any other parameter type/annotation combination is illegal, and corresponding exception will be
thrown at configuration time.

`@Pop*` annotations change `Stacks` state. This object is queried exactly in the order of parameters
annotated with stack-manipulating annotations. For example, the following annotated action:

```java
@Before(...)
public void action(@Pop String value1, @Pop int value2, @Peek boolean value3,
                   @Pop boolean value4) {
    ...
}
```

Is equivalent to the this direct stack manipulation:

```java
@Before(...)
public void action(Stacks stacks) {
    String value1 = stacks.pop();
    int value2 = stacks.pop();
    boolean value3 = stacks.peek();
    boolean value4 = stacks.pop();
    ...
}
```

It should be obvious that writing parameters annotated with `@Peek` annotation for the same stack
several times in a row is pointless, since they all will have the same value. On the other hand,
having several `@Pop`-annotated parameters is perfectly OK.

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
    protected void configure() {
        on("/persons")
            .doBefore(new ActionBefore() {
                @Override
                public void act(Stacks stacks, AttributesContext context) {
                    stacks.push(ImmutableList.builder());
                }
            })
            .doAfter(new ActionAfter() {
                @Override
                public void act(Stacks stacks, AttributesContext context) {
                    ImmutableList.Builder<Person> builder = stacks.pop();
                    stacks.push(builder.build());
                }
            });

        on("/persons/person")
            .doBefore(new ActionBefore() {
                @Override
                public void act(Stacks stacks, AttributesContext context) {
                    stacks.get("person").push(context.attribute("id").get());
                }
            })
            .doAfter(new ActionAfter() {
                @Override
                public void act(Stacks stacks, AttributesContext context) {
                    String name = stacks.get("person").pop();
                    int id = Integer.parseInt(stacks.get("person").<String>pop());
                    List<Login> logins = stacks.get("logins").<List<Login>>tryPop().or(ImmutableList.<Login>of());

                    ImmutableList.Builder<Person> personsBuilder = stacks.peek();
                    personsBuilder.add(new Person(id, name, logins));
                }
            });

        on("/persons/person/name")
            .doAt(new ActionAt() {
                @Override
                public void act(Stacks stacks, AttributesContext context, String body) {
                    stacks.get("person").push(body);
                }
            });

        on("/persons/person/logins")
            .doBefore(new ActionBefore() {
                @Override
                public void act(Stacks stacks, AttributesContext context) {
                    stacks.get("logins").push(ImmutableList.builder());
                }
            })
            .doAfter(new ActionAfter() {
                @Override
                public void act(Stacks stacks, AttributesContext context) {
                    stacks.get("logins").push(
                        stacks.get("logins").<ImmutableList.Builder<Login>>pop().build()
                    );
                }
            });

        on("/persons/person/logins/login")
            .doAt(new ActionAt() {
                @Override
                public void act(Stacks stacks, AttributesContext context, String body) {
                    stacks.get("logins").<ImmutableList.Builder<Login>>peek()
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
public class PersonModule2 extends AbstractMappingModule {
    @Override
    protected void configure() {
        on("/persons")
            .doBefore((stacks, context) -> stacks.push(ImmutableList.builder()))
            .doAfter((stacks, context) -> {
                ImmutableList.Builder<Person> builder = stacks.pop();
                stacks.push(builder.build());
            });

        on("/persons/person")
            .doBefore((stacks, context) -> stacks.get("person").push(context.attribute("id").get()))
            .doAfter((stacks, context) -> {
                String name = stacks.get("person").pop();
                int id = Integer.parseInt(stacks.get("person").<String>pop());
                List<Login> logins = stacks.get("logins").<List<Login>>tryPop().or(ImmutableList.<Login>of());

                ImmutableList.Builder<Person> personsBuilder = stacks.peek();
                personsBuilder.add(new Person(id, name, logins));
            });

        on("/persons/person/name")
            .doAt((stacks, context, body) -> stacks.get("person").push(body));

        on("/persons/person/logins")
            .doBefore((stacks, context) -> stacks.get("logins").push(ImmutableList.builder()))
            .doAfter((stacks, context) -> stacks.get("logins").push(
                stacks.get("logins").<ImmutableList.Builder<Login>>pop().build()
            ));

        on("/persons/person/logins/login")
            .doAt((stacks, context, body) -> {
                stacks.get("logins").<ImmutableList.Builder<Login>>peek()
                      .add(new Login(context.attribute("site").get(), body));
            });
    }
}
```

The only boilerplate code here is repeated use of `stacks`, `context` and `body` lambda parameters,
and even that is possible to alleviate using default methods provided by Java 8. I may possibly add
special Java 8 support library for this later.

In fact, IntelliJ IDEA folds code in such way that you see it very similar to this even with Java
6/7 language level.

Another possibility is annotated configuration. An annotated class which will do the same job as the
modules above looks like this:

```java
public class PersonAnnotatedConfig {
    @Before("/persons")
    public ImmutableList.Builder<Person> beforePersons() {
        return ImmutableList.builder();
    }

    @Before("/persons/person")
    @PushTo("person")
    public String personId(AttributesContext context) {
        return context.attribute("id").get();
    }

    @At("/persons/person/name")
    @PushTo("person")
    public String personName(String body) {
        return body;
    }

    @Before("/persons/person/logins")
    @PushTo("logins")
    public ImmutableList.Builder<Login> beforeLogins() {
        return ImmutableList.builder();
    }

    @At("/persons/person/logins/login")
    public void addLogin(@PeekFrom("logins") ImmutableList.Builder<Login> loginsBuilder, String body,
                         AttributesContext context) {
        loginsBuilder.add(new Login(context.attribute("site").get(), body));
    }

    @After("/persons/person/logins")
    @PushTo("logins")
    public List<Login> afterLogins(@PopFrom("logins") ImmutableList.Builder<Login> loginsBuilder) {
        return loginsBuilder.build();
    }

    @After("/persons/person")
    public void afterPerson(@PopFrom("person") String name, @PopFrom("person") String id,
                            @PopFrom("logins") Optional<List<Login>> logins,
                            @Peek ImmutableList.Builder<Person> personsBuilder) {
        personsBuilder.add(new Person(Integer.parseInt(id), name, logins.or(ImmutableList.<Login>of())));
    }

    @After("/persons")
    public List<Person> afterPersons(@Pop ImmutableList.Builder<Person> personsBuilder) {
        return personsBuilder.build();
    }
}
```

As you can see, annotated configuration reads slightly easier that modular configuration. There are
no explicit `Stacks` modification, it is hidden behind annotated parameters and methods.

Module definition was the hardest part. Actual XML parsing is very simple:

```java
Devourer devourer = Devourer.create(new PersonModule());
// Or:
// Devourer devourer = Devourer.create(new PersonAnnotatedConfig());
Reader reader = obtainReaderForXMLSomewhere();
Stacks stack = devourer.parse(reader);
List<Person> persons = stacks.pop();
```

That's it. `persons` will be a list of `Person` objects read from the XML and constructed via actions
configured in the module.

Operational configuration
-------------------------

It is possible to configure Devourer behavior using `DevourerConfig` object. These are the things
you can currently tweak using this configuration:

- enable or disable trimming of leading and trailing whitespaces of element textual content; this is
  something you usually want to have, so it is enabled by default, however, sometimes you want to
  turn it off (e.g. when handling CDATA);
- configure StAX parser directly by setting StAX `XMLInputFactory` properties.

`DevourerConfig` object is usually created using builder class. It looks like this:

```java
DevourerConfig config = DevourerConfig.builder()
    .setStripSpaces(false)
    .setStaxProperty(XMLInputFactory.IS_COALESCING, true)
    .build();
```

Then you can supply `DevourerConfig` object as an argument to `Devourer.create()` method:

```java
Devourer devourer = Devourer.create(config, new PersonModule());
```

Limitations of the library
--------------------------

There are several limitations of the library I'm aware of; some of them will be fixed in the next
versions.

1. Devourer does not handle namespaces well. Currently paths are resolved using local names; it is
   not possible to distinguish between different namespaces by the means of the path only. The only
   way you can use namespaces is to get them using `AttributesContext`. This is something I intend
   to change very soon.
2. Devourer does not have special shortcut actions to do some specific job, e.g. set JavaBean
   property or create an object. This is done intentionally, since such actions usually require
   reflection to work, and I tried to avoid reflection as much as possible. Also, I'm convinced that
   JavaBean-style class are evil: they are usually mutable (and I think that mutable DTOs should be
   avoided at all costs) and bring unneccessary clutter in form of getter/setter methods. However,
   Devourer may provide some shortcut actions in the future if I find them useful.
3. Devourer API is not stable right now. It is possible that I would change something in the new
   version which will break your code. I will notify about such changes in my blog.
   
Bugs
----

Currently I don't know of any bugs in the library. If you find one, feel free to post it to the
[Bitbucket issue tracker](https://bitbucket.org/googolplex/devourer/issues).
