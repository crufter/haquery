{-# LANGUAGE OverloadedStrings #-}

module Tests.Haquery where

import Haquery
import Test.HUnit
import qualified Data.Text as T

want what verdict = assertBool (show what) verdict

cases1 = [
    -- Id
    (div' [cat "id" "testId"] [],     "#testId",                    True),
    (div' [cat "id" "testId"] [],     "#testId1",                   False),
    -- Class
    (div' [cat "class" "testC"] [],     ".testC",                   True),
    (div' [cat "class" "testC"] [],     ".test1C",                  False),
    (div' [cat "class" "cl1 cl2 cl3"] [],     ".cl1",               True),
    (div' [cat "class" "cl1 cl2 cl3"] [],     ".cl2",               True),
    (div' [cat "class" "cl1 cl2 cl3"] [],     ".cl3",               True),
    (div' [cat "class" "cl1 cl2 cl3"] [],     ".cl4",               False),
    -- Attributes
    (div' [cat "href" "whatever"] [],   "[href=\"whatever\"]",      True),
    (div' [cat "href" "whatever"] [],   "[href=\"whaver\"]",        False),
    (div' [cat "href" "whatever"] [],   "[href=whatever]",          True),
    (div' [cat "href" "whatever"] [],   "[href=whaver]",            False),
    (div' [cat "href" "whatever"] [],   "[href^=\"what\"]",         True),
    (div' [cat "href" "whatever"] [],   "[href^=\"what1\"]",        False),
    (div' [cat "href" "whatever"] [],   "[href^=what]",             True),
    (div' [cat "href" "whatever"] [],   "[href^=what1]",            False),
    (div' [cat "href" "whatever"] [],   "[href$=\"ever\"]",         True),
    (div' [cat "href" "whatever"] [],   "[href$=\"ever1\"]",        False),
    (div' [cat "href" "whatever"] [],   "[href$=ever]",             True),
    (div' [cat "href" "whatever"] [],   "[href$=ever1]",            False),
    -- Composite
    (div' [cat "id" "tid", cat "class" "tc"] [],   "#tid.tc",       True),
    (div' [cat "id" "tid", cat "class" "tc"] [],   "#tid.tc1",      False),
    (div' [cat "id" "tid", cat "class" "tc"] [],   "#tid1.tc",      False),
    (div' [cat "id" "tid", cat "class" "tc"] [],   ".tc#tid",       True),
    (div' [cat "id" "tid", cat "class" "tc"] [],   ".tc1#tid",      False),
    (div' [cat "id" "tid", cat "class" "tc"] [],   ".tc#tid1",      False)
    ]

t cs = TestCase $ mapM_ f cs
    where
    f c =
        let shouldFind = e3 c
            source = e1 c
            selector = e2 c
        in want (source, selector) $ matches selector source == shouldFind

testMatches = t cases1

cases2 = [
    (
        div' [cat "id" "t1"] [
            div' [cat "id" "t2", cat "class" "tclass"] []
        ], [("#t2.tclass", 1), (".tclass#t2", 1)]
    ),
    (
        div' [cat "id" "t1", cat "class" "c1"] [
            div' [cat "id" "t2", cat "class" "c2"] [
                div' [cat "id" "t3", cat "class" "c3"] []
            ]
        ],
		[
			("#t1 #t2 #t3", 1),
			("#t1 #t3", 1),
			("#t1 > #t3", 0),
			("#t1.c1 #t3.c3", 1),
			("#t1.c11 #t3.c33", 0)
		]
    ),
    (
        div' [cat "id" "t1", cat "class" "c1"] [
            div' [cat "id" "t2", cat "class" "c2"] [
                div' [cat "id" "t3", cat "class" "c3"] [
                    div' [cat "id" "t4", cat "class" "c4"] []
                ]
            ],
            div' [cat "id" "t21", cat "class" "c21"] [
                div' [cat "id" "t31", cat "class" "c31"] [
                    div' [cat "id" "t41", cat "class" "c41"] []
                ]
            ]
        ],
        [
            ("#t1 #t4", 1),
            ("div:first-child", 5),
            ("div:last-child", 1),
            ("div:nth-child(2)", 1),
            (".c1:has(.c4)", 1),
            (":empty", 2),
            (":parent", 5),
            ("div:not(:parent)", 2),
            ("div:not(:empty)", 5),
            (".c1, .c2, .c21", 3)
        ]
    ),
    (
        html [cat "id" "t1"] [
            div' [cat "class" "c1"] [],
            div' [cat "class" "c2"] [],
            div' [cat "class" "c3"] [],
            div' [cat "class" "c4"] [],
            div' [cat "class" "c5"] [],
            div' [cat "class" "c6"] [],
            div' [cat "class" "c7"] [
                tag "li" [cat "class" "l1"] []
            ],
            div' [cat "class" "c8"] []
        ],
        [
            ("div", 8),
            ("div:eq(3)", 1),
            ("div:gt(0)", 7),
            ("div:gt(6)", 1),
            ("div:lt(0)", 0),
            ("div:lt(1)", 1),
            ("div:lt(7)", 7),
            ("div:first", 1),
            ("div:last", 1),
            ("div:even", 4),
            ("div:odd", 4),
            ("div:first-child", 1),
            ("div:last-child", 1),
            (":first-child", 2),
            (":last-child", 1),
            ("*:eq(0)", 1),
            -- This fails currently.
            (":eq(0)", 1),
            (":nth-child(1)", 2),
            ("div:nth-last-child(2)", 1),
            ("div:even:empty", 3),
            ("div:odd:first.c2", 1),
            ("* + .c5", 1),
            ("* ~ .c5", 1),
            (".c1 ~ div", 7),
            (".c4 ~ .c5", 1),
            (".c4 + .c5", 1),
            (".c4 ~ .c6", 1),
            (".c4 + .c6", 0)
        ]
    ),
    (
        body [] [
            div' [] [],
            tag "span" [cat "id" "prev"] [],
            div' [] [],
            div' [] [
                div' [cat "id" "small"] []
            ],
            tag "span" [] [],
            div' [] []
        ],
        [
            ("#prev ~ div", 3) -- This fails if there is only value equality implemented.
        ]
    ),
    -- Testing the aformentioned equality based selectors.
    (
        body [] [
            div' [] [],
            div' [] [],
            div' [] [
                div' [] [],
                div' [] [],
                div' [] [],
                div' [] [
                    div' [] [],
                    div' [] [],
                    div' [] [],
                    div' [] []
                ]
            ],
            div' [] [],
            div' [] []
        ],
        [
            ("div:first", 1),
            ("div:eq(5)", 1),
            ("div:last", 1),
            ("div:lt(3)", 3),
            ("div:gt(2)", 10)
        ]
    )
    ]

testSelect = TestCase $ mapM_ f cases2
    where
    f (tag, checks) = mapM_ f1 checks
        where
        f1 c =
            let selector        = fst c
                wantedMatches   = snd c
                actualMatches   = length $ select selector tag
            in want (c, actualMatches, tag) $ wantedMatches == actualMatches

cases3 = [
    div' [] [],
    div' ["class" -. "green blue"] [],
    div' [] [
        div' [] []
    ],
    div' ["class" -. "zee"] [
        tag "img" ["src" -. "x.jpg"] [],
        text "Hello, bello"
    ],
    div' [] [
        div' [] [],
        text "Yoh\n.",
        div' [] [
            div' [] [
                text "XYZ."
            ]
        ]
    ]
    ]

testParse = TestCase $ mapM_ f cases3
    where
    f a =
        let sh = render a
            parsed = parseHtml sh
            verdict = length parsed == 1 && (parsed!!0) == a
        in want (a, parsed) verdict

tests = TestList [
    TestLabel "testMatches" testMatches,
    TestLabel "testSelect" testSelect,
    TestLabel "testParse" testParse
    ]