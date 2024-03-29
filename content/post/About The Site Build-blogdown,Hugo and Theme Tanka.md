---
categories: ["English","introduce"]
tags: ["markdown","Hugo","theme","Hexo"]
author: pauke
date: "2019-01-18"
title: "About This Site Build: blogdown, Hugo and Theme"
---

## Why blogdown and Hugo
My initiate site build way is by Hexo. The [wreckage](https://enersto.github.io/) is still on. The theme and build process are enough satisfied, and I also got to learn things about git and github basically in the process. But when I want actually to post my article, which is  based on Rmarkdown, I find [blogdown](https://bookdown.org/yihui/blogdown/), the product both comes from [Yihui](https://yihui.name/cn/about/),Rstudio, whom My R learning process benefits superbly from.
The profound tutorial material and theme libraries, super family GUI tool(Rstudio) and seem fast build process(heard the fast character of go before). These are my thought about the blogdown after read the [book](https://bookdown.org/yihui/blogdown/).
So why not switch my site build to hugo. 
As for the building workflow, I think blogdown has optimized so extremely that you can just dive in without any web site and deployment knowledge, and the [article](https://bookdown.org/yihui/blogdown/workflow.html) make me think that I have no need to build wheel again.


## About the Theme
My site is based on the theme [lithium](https://github.com/jrutheiser/hugo-lithium-theme), which is a totally simple, clean and word orientation Hugo theme.
On the other, the theme is so simple that I need to enrich more functions to feed my blog needs by myself.It is the way I customize this theme,and also the way I get learning things about Hugo, css and HTML. And this is the parts I change or add:

### taxonomies index
Tanka has not  tags and categories index, but the social account line. And I think an achieved index is necessary for a blog site. And I fork the code of taxonomies from [Xmin](https://github.com/yihui/hugo-xmin).

### table format
There is no table setting in the theme too, and my site also gets code from Xmin.

| Sepal.Length| Sepal.Width| Petal.Length| Petal.Width|Species |
|------------:|-----------:|------------:|-----------:|:-------|
|          5.1|         3.5|          1.4|         0.2|setosa  |
|          4.9|         3.0|          1.4|         0.2|setosa  |
|          4.7|         3.2|          1.3|         0.2|setosa  |

### comments system
For the visitor beneath the GFW, I choose the [utteranc](https://utteranc.es/?installation_id=31085135&setup_action=install) to get a communicate platform in a easy and security way. In this [post](https://mscipio.github.io/post/utterances-comment-engine/), the method to install in hugo is presented so clearly.

Though disqus loses some reputation recently, but for a cozy way to show feeling about the article, I still keep the disqus then. The comments system of original theme is hugo embedded comments, Disqus. I don't find much articles comparing of comments in Hugo, which recommends [these comments system](https://gohugo.io/content-management/comments/#comments-alternatives) on documents. But Hexo and other static site generators articles are also sightly:

- [Various ways to include comments on your static site](https://darekkay.com/blog/static-site-comments/) 
- [第三方评论系统推荐](https://3mile.github.io/archives/128/).

~~And my demand of comments is: **simple and lightweight, self-host, anonymous and not blocked in China**. And [valine](https://valine.js.org/en/) is just fitted. And this is an good [article](https://www.smslit.top/2018/07/08/hugo-valine/) to maintain the build way in Hugo.~~  (valine's background requires ID verified now, I have to give it up )

### mermaid
[Mermaid](https://mermaidjs.github.io/) is a simple and useful markdown-like script language for generating charts from text via javascript.

{{<mermaid>}}
sequenceDiagram
    participant Alice
    participant Bob
    Alice->>John: Hello John, how are you?
    loop Healthcheck
        John->John: Fight against hypochondria
    end
    Note right of John: Rational thoughts <br/>prevail...
    John-->Alice: Great!
    John->Bob: How about you?
    Bob-->John: Jolly good!
{{< /mermaid >}}

My site mermaid function is forked from the theme [learn](https://github.com/matcornic/hugo-theme-learn). This is the method you can add the function on your theme:

- get the `mermaid.html`  from `hugo-theme-learn\layouts\shortcodes` to `your_theme_file\layouts\shortcodes`;
- fork the "mermaid file" in `hugo-theme-learn\static\` to `your_theme_file\static\`, which contains 4 files.  

### highlight of code
About the highlight setting for blog build, this [article](https://amber.rbind.io/blog/2017/11/15/syntax-highlighting/) interprets very profound.
My highlight style choice is the "Tomorrow".And this is the highlight demo:

```r
library(ggplot2)

centre <- function(x, type, ...) {
  switch(type,
         mean = mean(x),
         median = median(x),
         trimmed = mean(x, trim = .1))
}

data$x
foo "bar" baz
# test "test"
"test # test"

(123) (1) (10) (0.1) (.2) (1e-7)
(3.) (3.E10) # BUG: .E10 should be part of number

plot(cars, xlim=20)
plot(cars, xlim=0x20)
foo<-30
my.data.3 <- read() # not a number
c(1,2,3)
1%%2
```
### table of contents

The smart table of contents function(smartToc), which is forked from this theme [minos](https://github.com/carsonip/hugo-theme-minos), is deep into 6th. header, and is fixed when rolling the page.
You can get this function in this way:

 - fork the function part file on the this [repo](https://github.com/enersto/customized_hugo_theme/blob/master/themes/hugo-tanka/layouts/partials/smart_toc.html), and lay down on the `your_theme_name/layouts/partials/`;
 
 - leave a partial html function on `your_theme_name/layouts/_default/single.html`(you also can get the code on my repo file); 
 
 - set style on the `your_theme_name/static/css/style.css` about the toc.

