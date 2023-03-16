[<AutoOpen>]
module internal FsReveal.Markdown

open System
open FSharp.Literate
open FSharp.Markdown

/// Old style DU without the ranges
type FsRevealMarkdownParagraph =
| Heading of size: int * body: MarkdownSpans
| Paragraph of body: MarkdownSpans
| CodeBlock of code: string * language: string * ignoredLine: string
| InlineBlock of code: string
| ListBlock of kind: MarkdownListKind * items: MarkdownParagraphs list
| QuotedBlock of paragraphs: MarkdownParagraphs
| Span of body: MarkdownSpans
| LatexBlock of body: string list
| HorizontalRule of character: char
| TableBlock of headers: MarkdownTableRow option * alignments: MarkdownColumnAlignment list * rows: MarkdownTableRow list
| EmbedParagraphs of customParagraphs: MarkdownEmbedParagraphs

/// Transforms the new DU into the old style DU without ranges
let parasToFsParas (paraList:FSharp.Markdown.MarkdownParagraphs) =
    paraList |>
    List.map (function
        | FSharp.Markdown.Heading(l, body, _)                       -> Heading(l, body)
        | FSharp.Markdown.Paragraph(body, _)                        -> Paragraph(body)
        | FSharp.Markdown.CodeBlock(code,lang, ignored, _)          -> CodeBlock(code,lang, ignored)
        | FSharp.Markdown.InlineBlock(code, _)                      -> InlineBlock(code)
        | FSharp.Markdown.ListBlock(kind, items, _)                 -> ListBlock(kind, items)
        | FSharp.Markdown.QuotedBlock(paragraphs, _)                -> QuotedBlock(paragraphs)
        | FSharp.Markdown.Span(l, _)                                -> Span(l)
        | FSharp.Markdown.LatexBlock(body, _)                       -> LatexBlock(body)
        | FSharp.Markdown.HorizontalRule(character, _)              -> HorizontalRule(character)
        | FSharp.Markdown.TableBlock(headers, aligments, rows, _)   -> TableBlock(headers, aligments, rows)
        | FSharp.Markdown.EmbedParagraphs(customParagraphs, _)      -> EmbedParagraphs(customParagraphs)
    )

/// Transforms the old style DU into the new style DU, ranges are set to None and are lost.
// Let's hope they are not really used
let fsParasToParas (paraList:FsRevealMarkdownParagraph list) =
    paraList |>
    List.map (function
        | Heading(l, body)                      -> FSharp.Markdown.Heading(l, body, None)
        | Paragraph(body)                       -> FSharp.Markdown.Paragraph(body, None)
        | CodeBlock(code,lang, ignored)         -> FSharp.Markdown.CodeBlock(code,lang, ignored, None)
        | InlineBlock(code)                     -> FSharp.Markdown.InlineBlock(code, None)
        | ListBlock(kind, items)                -> FSharp.Markdown.ListBlock(kind, items, None)
        | QuotedBlock(paragraphs)               -> FSharp.Markdown.QuotedBlock(paragraphs, None)
        | Span(l)                               -> FSharp.Markdown.Span(l, None)
        | LatexBlock(body)                      -> FSharp.Markdown.LatexBlock(body, None)
        | HorizontalRule(character)             -> FSharp.Markdown.HorizontalRule(character, None)
        | TableBlock(headers, aligments, rows)  -> FSharp.Markdown.TableBlock(headers, aligments, rows, None)
        | EmbedParagraphs(customParagraphs)     -> FSharp.Markdown.EmbedParagraphs(customParagraphs, None)
    )


let getPresentation (doc : LiterateDocument) = 
    /// get properties, a list of (key,value) from
    /// [[Span[Literal "key : value"]]]
    let getProperties (spans : list<list<_>>) = 
        let extractProperty paragraphs = 
            match paragraphs with
            | [ Span(l) ] -> 
                match l with
                | [ Literal(v, _) ] when v.Contains(":") -> 
                    let colonPos = v.IndexOf(':')
                    let key = v.Substring(0, colonPos).Trim()
                    let value = v.Substring(colonPos + 1).Trim()
                    (key, value)
                | _ -> failwithf "Invalid Presentation property: %A" l
            | _ -> failwithf "Invalid Presentation property: %A" paragraphs
        spans |> List.map extractProperty
    
    // main section is separated by ***
    let sections = splitBy (HorizontalRule('*')) (doc.Paragraphs |> parasToFsParas)
    
    let properties,slideData =
        let map,slideData =
            match sections.Head with
            | [ ListBlock(_, spans) ] -> getProperties (spans |> List.map parasToFsParas) |> Map.ofList,sections.Tail
            | x -> Map.empty,sections

        let add key v map =
            match Map.tryFind key map with
            | None -> Map.add key v map
            | _ -> map

        let properties = 
            map
            |> add "title" "Presentation"
            |> add "description" ""
            |> add "author" "unkown"
            |> add "theme" "night"
            |> add "transition" "default"
        properties,slideData
    
    let wrappedInSection (properties:Map<_,_>) paragraphs = 
        let attributes = properties |> Seq.map (fun kv -> sprintf "%s=\"%s\"" kv.Key kv.Value)
        InlineBlock(sprintf "<section %s>" (String.Join(" ", attributes))) :: paragraphs @ [ InlineBlock("</section>") ]
    
    let getParagraphsFromSlide slide = 
        match slide with
        | Simple(slideData)
        | Nested([slideData]) -> wrappedInSection slideData.Properties (slideData.Paragraphs |> parasToFsParas)
        | Nested(nestedSlides) -> 
            nestedSlides
            |> List.collect (fun slideData -> wrappedInSection slideData.Properties (slideData.Paragraphs |> parasToFsParas))
            |> wrappedInSection Map.empty
            
    let extractSlide paragraphs = 
        let extractSlideData paragraphs = 
            let properties, data =
                match paragraphs with
                | ListBlock(_, spans) :: data -> 
                    try 
                        getProperties (spans |> List.map parasToFsParas), data
                    with _ -> [], paragraphs
                | _ -> [], paragraphs

            { Properties = properties |> Map.ofList
              Paragraphs = (data |> fsParasToParas)}

        // sub-section is separated by ---
        let nestedSlides =
            paragraphs
            |> splitBy (HorizontalRule('-'))
            |> List.map extractSlideData

        match nestedSlides with
        | [ slideData ] -> Simple slideData
        | _ -> Nested nestedSlides
    
    let slides = List.map extractSlide slideData
    let paragraphs = List.collect getParagraphsFromSlide slides
    { Properties = properties
      Slides = slides
      Document = doc.With(paragraphs = (paragraphs |> fsParasToParas)) }
