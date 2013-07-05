Haquery
=====

### What is this?
Haquery is **jQuery for Haskell**, a CSS selector based manipulation and template engine.

### Why?
The advantage is that most web developers already know CSS selectors, and if you are one of them,  
you don't have to learn a new template language just to render data as HTML pages.

Also, you can use any data structure, any functions with Haquery.  
No more "if there is an if construct why there is no else" moment like with many other template
languages.

### Installation
This package is not Hackage currently, so you have to clone this repo and issue a cabal install locally.

### Selectors

    Example                     Name                                    URL
    *                           - All selector                          http://api.jquery.com/all-selector/
    #id                         - Id selector                           http://api.jquery.com/id-selector/
    .class                      - Class selector                        http://api.jquery.com/class-selector/
    selector1 selector2         - Descendant selector                   http://api.jquery.com/descendant-selector/
    type                        - Element selector                      http://api.jquery.com/element-selector/
    selector1 > selector2       - Child selector                        http://api.jquery.com/child-selector/
    [attrName]                  - Has attribute selector                http://api.jquery.com/has-attribute-selector/
    [attrName="val"]            - Attribute equals selector             http://api.jquery.com/attribute-equals-selector/
    [attrName*="val"]           - Attribute contains selector           http://api.jquery.com/attribute-contains-selector/
    [attrName^="val"]           - Attribute starts with selector        http://api.jquery.com/attribute-starts-with-selector/
    [attrName$="val"]           - Attribute ends with selector          http://api.jquery.com/attribute-ends-with-selector/
    [attrName~="val"]           - Attribute contains word selector      http://api.jquery.com/attribute-contains-word-selector/
    [attrName!="val"]           - Attribute not equals selector         http://api.jquery.com/attribute-not-equal-selector/
    selector1, selector2        - Multiple selectors selector           http://api.jquery.com/multiple-selector/
    prev + next                 - Next adjacent selector                http://api.jquery.com/next-adjacent-selector/
    prev ~ siblings             - Next siblings selector                http://api.jquery.com/next-siblings-selector/
    :not(selector)              - :not() selector                       http://api.jquery.com/not-selector/
    :has(selector)              - :has() selector                       http://api.jquery.com/has-selector/
    :eq(3)                      - :eq() selector                        http://api.jquery.com/eq-selector/
    :lt(3)                      - :lt() selector                        http://api.jquery.com/lt-selector/
    :gt(3)                      - :gt() selector                        http://api.jquery.com/gt-selector/
    :even                       - :even selector                        http://api.jquery.com/even-selector/
    :odd                        - :odd selector                         http://api.jquery.com/odd-selector/
    :first                      - :first selector                       http://api.jquery.com/first-selector/
    :last                       - :last selector                        http://api.jquery.com/last-selector/
    :first-child                - :first-child selector                 http://api.jquery.com/first-child-selector/
    :last-child                 - :last-child selector                  http://api.jquery.com/last-child-selector/
    :nth-child(3)               - :nth-child() selector                 http://api.jquery.com/nth-child-selector/
    :nth-last-child(3)          - :nth-last-child() selector            http://api.jquery.com/nth-last-child-selector/
    :empty                      - :empty selector                       http://api.jquery.com/empty-selector/
    :parent                     - :parent selector                      http://api.jquery.com/parent-selector/

### Examples

We can write the HTML data structures in Haskell, or we can parse them from HTML.    
Now we will focus on the former.
It is very simple indeed: an HTML tag is nothing else but a tagname, some key value pairs and children:

```haskell
--  Tagname     Attribute 1         Attribute 2         Children
    body        [cat "id" "x",      cat "class" "y"]    [
        div' [] [], -- Child 1 with no attributes, no children
        div' [] []  -- Child 2 also with no attributes and no children
    ]
```

'cat' means create attribute. Note that the names are not final. Any suggestion is welcome.  
div became div', and head is called head' to avoid clash with prelude.

If you want, you can use the general tag form too:

```haskell
    tag "body" [cat "id" "x", cat "class" "y"] [
        tag "div" [] [],
        tag "div" [] []
    ]
```

Now let's look at some complete examples.  

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Haquery

example :: Tag
example = html [
        head' [] [],
        body [] [
            div' [cat "id" "main", cat "class" "c1"] [
                div' [cat "id" "sub1"] [],
                div' [cat "id" "sub2"] []
            ]
        ]
    ]
```

The above example is identical to the following HTML snippet:

```html
<html>
    <head></head>
    <body>
        <div id="main" class="c1">
            <div id="sub1"></div>
            <div id="sub2"></div>
        </div>
    </body>
</html>
```

Now we can start querying and manipulating our 'example'.

Let's do a selection.

```haskell
> select "#main" example
[<div id="main" class="c1">
    <div id="sub1"></div>
    <div id="sub2"></div>
</div>]
```

A selection returns all tags matching the given selector.  
Obviously, we can't modify these values due to Haskell's purity.
If we want to modify tags matching a selector, we can use the alter function.

```haskell
> :t alter
alter :: T.Text -> Tag -> (Tag -> Tag) -> Tag
```

As we can see, the alter functions needs a selector, a tag to search in, and a function which transforms
the matching tags. Let's add a class to every div! We can do this with the addClass method.

```haskell
> :t addClass
addClass :: T.Text -> Tag -> Tag
```

```haskell
> alter "div" example (addClass "hello-class")
<html>
    <head></head>
    <body>
        <div id="main" class="c1 hello-class">
            <div id="sub1" class="hello-class"></div>
            <div id="sub2" class="hello-class"></div>
        </div>
    </body>
</html>
```

More coming soon.