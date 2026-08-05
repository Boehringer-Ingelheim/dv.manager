# filter_to_export works with an empty filter

    Code
      filter_to_export(udlwfi)
    Output
      $txt
      [1] "Subject filter\n\ndf1_label [df1] (2 out of 2 ─ 0 rows dropped)\n\ndf2_label [df2] (1 out of 2 ─ 1 rows dropped)\n"
      
      $reference_txt
      [1] ""
      

# filter_to_export works with all actions

    Code
      filter_to_export(udlwfi)[["txt"]]
    Output
      [1] "Subject filter\n└─ union\n   └─ intersect\n      └─ complement\n         └─ not\n            └─ and\n               └─ or\n                  ├─ Variable: a_label [a]\n                  │  ├─ Dataset: df1_label [df1]\n                  │  ├─ Min: 2\n                  │  ├─ Max: 4\n                  │  └─ Include NA: FALSE\n                  │ \n                  └─ Variable: b_label [b]\n                     ├─ Dataset: df1_label [df1]\n                     ├─ Selected 26 value(s)\n                     │  ├─ a\n                     │  ├─ b\n                     │  ├─ c\n                     │  ├─ d\n                     │  ├─ e\n                     │  └─ ... (21 values not shown. See filter reference (1))\n                     └─ Include NA: FALSE\n                    \n\ndf1_label [df1] (2 out of 2 ─ 0 rows dropped)\n└─ not\n   └─ and\n      └─ or\n         ├─ Variable: a_label [a]\n         │  ├─ Dataset: df1_label [df1]\n         │  ├─ Min: 2\n         │  ├─ Max: 4\n         │  └─ Include NA: FALSE\n         │ \n         └─ Variable: b_label [b]\n            ├─ Dataset: df1_label [df1]\n            ├─ Selected 26 value(s)\n            │  ├─ a\n            │  ├─ b\n            │  ├─ c\n            │  ├─ d\n            │  ├─ e\n            │  └─ ... (21 values not shown. See filter reference (2))\n            └─ Include NA: FALSE\n           \n\ndf2_label [df2] (1 out of 2 ─ 1 rows dropped)\n"

---

    Code
      message(filter_to_export(udlwfi)[["txt"]])
    Message
      Subject filter
      └─ union
         └─ intersect
            └─ complement
               └─ not
                  └─ and
                     └─ or
                        ├─ Variable: a_label [a]
                        │  ├─ Dataset: df1_label [df1]
                        │  ├─ Min: 2
                        │  ├─ Max: 4
                        │  └─ Include NA: FALSE
                        │ 
                        └─ Variable: b_label [b]
                           ├─ Dataset: df1_label [df1]
                           ├─ Selected 26 value(s)
                           │  ├─ a
                           │  ├─ b
                           │  ├─ c
                           │  ├─ d
                           │  ├─ e
                           │  └─ ... (21 values not shown. See filter reference (1))
                           └─ Include NA: FALSE
                          
      
      df1_label [df1] (2 out of 2 ─ 0 rows dropped)
      └─ not
         └─ and
            └─ or
               ├─ Variable: a_label [a]
               │  ├─ Dataset: df1_label [df1]
               │  ├─ Min: 2
               │  ├─ Max: 4
               │  └─ Include NA: FALSE
               │ 
               └─ Variable: b_label [b]
                  ├─ Dataset: df1_label [df1]
                  ├─ Selected 26 value(s)
                  │  ├─ a
                  │  ├─ b
                  │  ├─ c
                  │  ├─ d
                  │  ├─ e
                  │  └─ ... (21 values not shown. See filter reference (2))
                  └─ Include NA: FALSE
                 
      
      df2_label [df2] (1 out of 2 ─ 1 rows dropped)
      

---

    Code
      message(filter_to_export(udlwfi)[["reference_txt"]])
    Message
      Reference (1): 
      "a"	"b"	"c"	"d"	"e"	"f"	"g"	"h"	"i"	"j"	"k"	"l"	"m"	"n"	"o"	"p"	"q"	"r"	"s"	"t"	"u"	"v"	"w"	"x"	"y"	"z"
      
      Reference (2): 
      "a"	"b"	"c"	"d"	"e"	"f"	"g"	"h"	"i"	"j"	"k"	"l"	"m"	"n"	"o"	"p"	"q"	"r"	"s"	"t"	"u"	"v"	"w"	"x"	"y"	"z"
      

