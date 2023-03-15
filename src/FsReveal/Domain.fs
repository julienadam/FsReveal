﻿namespace FsReveal

open FSharp.Literate
open FSharp.Markdown

type SlideData = 
    { Properties : Map<string,string>
      Paragraphs : MarkdownParagraph list }

type Slide = 
    | Simple of SlideData
    | Nested of SlideData list

type Presentation = 
    { Properties : Map<string,string>
      Slides : Slide list
      Document : LiterateDocument }
