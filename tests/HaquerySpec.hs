{-# LANGUAGE OverloadedStrings #-}

module HaquerySpec (main, spec) where

import qualified Data.Text  as T
import           Haquery
import           Test.Hspec

cases1 :: [(Tag, T.Text, Bool)]
cases1 = [
    -- Id
    (div' [at "id" "testId"] [],     "#testId",                    True),
    (div' [at "id" "testId"] [],     "#testId1",                   False),
    -- Class
    (div' [at "class" "testC"] [],     ".testC",                   True),
    (div' [at "class" "testC"] [],     ".test1C",                  False),
    (div' [at "class" "cl1 cl2 cl3"] [],     ".cl1",               True),
    (div' [at "class" "cl1 cl2 cl3"] [],     ".cl2",               True),
    (div' [at "class" "cl1 cl2 cl3"] [],     ".cl3",               True),
    (div' [at "class" "cl1 cl2 cl3"] [],     ".cl4",               False),
    -- Attributes
    (div' [at "href" "whatever"] [],   "[href=\"whatever\"]",      True),
    (div' [at "href" "whatever"] [],   "[href=\"whaver\"]",        False),
    (div' [at "href" "whatever"] [],   "[href=whatever]",          True),
    (div' [at "href" "whatever"] [],   "[href=whaver]",            False),
    (div' [at "href" "whatever"] [],   "[href^=\"what\"]",         True),
    (div' [at "href" "whatever"] [],   "[href^=\"what1\"]",        False),
    (div' [at "href" "whatever"] [],   "[href^=what]",             True),
    (div' [at "href" "whatever"] [],   "[href^=what1]",            False),
    (div' [at "href" "whatever"] [],   "[href$=\"ever\"]",         True),
    (div' [at "href" "whatever"] [],   "[href$=\"ever1\"]",        False),
    (div' [at "href" "whatever"] [],   "[href$=ever]",             True),
    (div' [at "href" "whatever"] [],   "[href$=ever1]",            False),
    -- Composite
    (div' [at "id" "tid", at "class" "tc"] [],   "#tid.tc",       True),
    (div' [at "id" "tid", at "class" "tc"] [],   "#tid.tc1",      False),
    (div' [at "id" "tid", at "class" "tc"] [],   "#tid1.tc",      False),
    (div' [at "id" "tid", at "class" "tc"] [],   ".tc#tid",       True),
    (div' [at "id" "tid", at "class" "tc"] [],   ".tc1#tid",      False),
    (div' [at "id" "tid", at "class" "tc"] [],   ".tc#tid1",      False)
    ]


cases2 :: [(Tag, [(T.Text, Int)])]
cases2 = [
    (
        div' [at "id" "t1"] [
            div' [at "id" "t2", at "class" "tclass"] []
        ], [("#t2.tclass", 1), (".tclass#t2", 1)]
    ),
    (
        div' [at "id" "t1", at "class" "c1"] [
            div' [at "id" "t2", at "class" "c2"] [
                div' [at "id" "t3", at "class" "c3"] []
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
        div' [at "id" "t1", at "class" "c1"] [
            div' [at "id" "t2", at "class" "c2"] [
                div' [at "id" "t3", at "class" "c3"] [
                    div' [at "id" "t4", at "class" "c4"] []
                ]
            ],
            div' [at "id" "t21", at "class" "c21"] [
                div' [at "id" "t31", at "class" "c31"] [
                    div' [at "id" "t41", at "class" "c41"] []
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
        html [at "id" "t1"] [
            div' [at "class" "c1"] [],
            div' [at "class" "c2"] [],
            div' [at "class" "c3"] [],
            div' [at "class" "c4"] [],
            div' [at "class" "c5"] [],
            div' [at "class" "c6"] [],
            div' [at "class" "c7"] [
                tag "li" [at "class" "l1"] []
            ],
            div' [at "class" "c8"] []
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
            --(":eq(0)", 1),
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
            tag "span" [at "id" "prev"] [],
            div' [] [],
            div' [] [
                div' [at "id" "small"] []
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

cases3 :: [Tag]
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


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Haquery" $ do
    it "test matches" $ do
      mapM_ testMatches cases1
    it "test select" $ do
      mapM_ testSelect cases2
    it "test parseHtml" $ do
      mapM_ testParseHtml cases3


testMatches :: (Tag, T.Text, Bool) -> Expectation
testMatches (tag, selector, shouldMatch) =
  matches selector tag `shouldBe` Right shouldMatch

testParseHtml :: Tag -> Expectation
testParseHtml tag =
  -- TODO: use QuickCheck
  parseHtml (render tag) `shouldBe` Right [tag]

testSelect :: (Tag, [(T.Text, Int)]) -> Expectation
testSelect (tag, selects) = mapM_ f selects
  where
    f (selector, numMatches) = length <$> (select selector tag) `shouldBe` Right numMatches
