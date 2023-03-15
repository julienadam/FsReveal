// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------

#r @"packages/build/FAKE/tools/FakeLib.dll"

open Fake
open Fake.Git
open Fake.DotNet.Testing
open Fake.AssemblyInfoFile
open Fake.ReleaseNotesHelper
open System
open System.IO
#if MONO
#else
#load "packages/build/SourceLink.Fake/Tools/Fake.fsx"
open SourceLink
#endif

// --------------------------------------------------------------------------------------
// START TODO: Provide project-specific details below
// --------------------------------------------------------------------------------------

// Information about the project are used
//  - for version and project name in generated AssemblyInfo file
//  - by the generated NuGet package
//  - to run tests and to publish documentation on GitHub gh-pages
//  - for documentation, you also need to edit info in "docs/tools/generate.fsx"

// The name of the project
// (used by attributes in AssemblyInfo, name of a NuGet package and directory in 'src')
let project = "FsReveal"

// Short summary of the project
// (used as description in AssemblyInfo and as a short summary for NuGet package)
let summary = "FsReveal parses markdown or F# script files and generates reveal.js slides."

// Longer description of the project
// (used as a description for NuGet package; line breaks are automatically cleaned up)
let description = "FsReveal parses markdown or F# script files and generates reveal.js slides."

// List of author names (for NuGet package)
let authors = [ "Karlkim Suwanmongkol" ]

// Tags for your project (for NuGet package)
let tags = "F#, markdown, reveal.js"

// File system information
let solutionFile  = "FsReveal.sln"

// Pattern specifying assemblies to be tested using NUnit
let testAssemblies = "tests/**/bin/Release/*Tests*.dll"

// Git configuration (used for publishing documentation in gh-pages branch)
// The profile where the project is posted
let gitOwner = "fsprojects"
let gitHome = "https://github.com/" + gitOwner

// The name of the project on GitHub
let gitName = "FsReveal"

// The url for the raw files hosted
let gitRaw = environVarOrDefault "gitRaw" "https://raw.github.com/fsprojects"

// --------------------------------------------------------------------------------------
// END TODO: The rest of the file includes standard build steps
// --------------------------------------------------------------------------------------

let buildDir = "bin"
// Read additional information from the release notes document
let release = LoadReleaseNotes "RELEASE_NOTES.md"

let outDir = "./docs/output"

let genFSAssemblyInfo (projectPath) =
    let projectName = System.IO.Path.GetFileNameWithoutExtension(projectPath)
    let basePath = "src/" + projectName
    let fileName = basePath + "/AssemblyInfo.fs"
    CreateFSharpAssemblyInfo fileName
      [ Attribute.Title (projectName)
        Attribute.Product project
        Attribute.Description summary
        Attribute.Version release.AssemblyVersion
        Attribute.FileVersion release.AssemblyVersion ]

let genCSAssemblyInfo (projectPath) =
    let projectName = System.IO.Path.GetFileNameWithoutExtension(projectPath)
    let basePath = "src/" + projectName + "/Properties"
    let fileName = basePath + "/AssemblyInfo.cs"
    CreateCSharpAssemblyInfo fileName
      [ Attribute.Title (projectName)
        Attribute.Product project
        Attribute.Description summary
        Attribute.Version release.AssemblyVersion
        Attribute.FileVersion release.AssemblyVersion ]

// Generate assembly info files with the right version & up-to-date information
Target "AssemblyInfo" (fun _ ->
  let fsProjs =  !! "src/**/*.fsproj"
  let csProjs = !! "src/**/*.csproj"
  fsProjs |> Seq.iter genFSAssemblyInfo
  csProjs |> Seq.iter genCSAssemblyInfo
)

// --------------------------------------------------------------------------------------
// Clean build results

Target "Clean" (fun _ ->
    CleanDirs [buildDir; "temp"]
)

Target "CleanDocs" (fun _ ->
    CleanDirs [outDir]
)

// --------------------------------------------------------------------------------------
// Build library & test project

Target "Build" (fun _ ->
    !! solutionFile
    |> MSBuildRelease "" "Rebuild"
    |> ignore
)

// --------------------------------------------------------------------------------------
// Run the unit tests using test runner

Target "RunTests" (fun _ ->
    let workDir = Environment.CurrentDirectory
    try
        !! testAssemblies
        |> NUnit3.run (fun p ->
            { p with
                ShadowCopy = false
                TimeOut = TimeSpan.FromMinutes 20.
                WorkingDir = "tests/FsReveal.Tests"
            })
    finally
        Environment.CurrentDirectory <- workDir
)

#if MONO
#else
// --------------------------------------------------------------------------------------
// SourceLink allows Source Indexing on the PDB generated by the compiler, this allows
// the ability to step through the source code of external libraries https://github.com/ctaggart/SourceLink

Target "SourceLink" (fun _ ->
    let baseUrl = sprintf "%s/%s/{0}/%%var2%%" gitRaw (project.ToLower())
    use repo = new GitRepo(__SOURCE_DIRECTORY__)
    !! "src/**/*.fsproj"
    |> Seq.iter (fun f ->
        let proj = VsProj.LoadRelease f
        logfn "source linking %s" proj.OutputFilePdb
        let files = proj.Compiles -- "**/AssemblyInfo.fs"
        repo.VerifyChecksums files
        proj.VerifyPdbChecksums files
        proj.CreateSrcSrv baseUrl repo.Revision (repo.Paths files)
        Pdbstr.exec proj.OutputFilePdb proj.OutputFilePdbSrcSrv
    )
)
#endif

// --------------------------------------------------------------------------------------
// Build a NuGet package

Target "NuGet" (fun _ ->    
    Paket.Pack (fun p -> 
        { p with 
            Version = release.NugetVersion
            ReleaseNotes = toLines release.Notes })
)

Target "PublishNuGet" (fun _ ->
    Paket.Push id
)

// --------------------------------------------------------------------------------------
// Generate the documentation

let executeFSIFromToolsPackageWithArgs workdir script (args:string list) =
    let workDir = Environment.CurrentDirectory
    try
        let path = Fake.EnvironmentHelper.normalizePath workdir
        Environment.CurrentDirectory <- path
        let fsiPath = __SOURCE_DIRECTORY__ @@ "packages" @@ "build" @@ "FSharp.Compiler.Tools" @@ "tools" @@ "fsi.exe"
        if not <| File.Exists(fsiPath) then
            failwithf "FSI not found"
        let argsConcatenated = String.Join(" ", script :: args)
        Fake.ProcessHelper.ExecProcess (fun (p:Diagnostics.ProcessStartInfo) -> 
            p.FileName <- fsiPath
            p.Arguments <- argsConcatenated
        ) (TimeSpan.FromMinutes(3.0)) = 0
    finally
        Environment.CurrentDirectory <- workDir

Target "GenerateReferenceDocs" (fun _ ->
    if not <| executeFSIFromToolsPackageWithArgs "docs/tools" "generate.fsx" ["--define:RELEASE"; "--define:REFERENCE"] then
      failwith "generating reference documentation failed"
)

let generateHelp fail =
    if executeFSIFromToolsPackageWithArgs "docs/tools" "generate.fsx" ["--define:RELEASE"; "--define:HELP"] then
        traceImportant "Help generated"
    else
        if fail then
            failwith "generating help documentation failed"
        else
            traceImportant "generating help documentation failed"
    

Target "GenerateHelp" (fun _ ->
    DeleteFile "docs/content/release-notes.md"    
    CopyFile "docs/content/" "RELEASE_NOTES.md"
    Rename "docs/content/release-notes.md" "docs/content/RELEASE_NOTES.md"

    DeleteFile "docs/content/license.md"
    CopyFile "docs/content/" "LICENSE.txt"
    Rename "docs/content/license.md" "docs/content/LICENSE.txt"

    generateHelp true
)


Target "GenerateSlides" (fun _ ->
    if executeFSIFromToolsPackageWithArgs "docs/tools" "createSlides.fsx" [] then
        traceImportant "Slides generated"
)


Target "KeepRunning" (fun _ ->    
    use watcher = new FileSystemWatcher(DirectoryInfo("docs/content").FullName,"*.*")
    watcher.EnableRaisingEvents <- true
    watcher.Changed.Add(fun e -> generateHelp false)
    watcher.Created.Add(fun e -> generateHelp false)
    watcher.Renamed.Add(fun e -> generateHelp false)
    watcher.Deleted.Add(fun e -> generateHelp false)

    traceImportant "Waiting for help edits. Press any key to stop."

    System.Console.ReadKey() |> ignore

    watcher.EnableRaisingEvents <- false
    watcher.Dispose()
)

Target "GenerateDocs" DoNothing

// --------------------------------------------------------------------------------------
// Release Scripts

Target "ReleaseDocs" (fun _ ->
    let tempDocsDir = "temp/gh-pages"
    CleanDir tempDocsDir
    Repository.cloneSingleBranch "" (gitHome + "/" + gitName + ".git") "gh-pages" tempDocsDir

    fullclean tempDocsDir
    CopyRecursive "docs/output" tempDocsDir true |> tracefn "%A"
    StageAll tempDocsDir
    Git.Commit.Commit tempDocsDir (sprintf "Update generated documentation for version %s" release.NugetVersion)
    Branches.push tempDocsDir
)

//#load "paket-files/build/fsharp/FAKE/modules/Octokit/Octokit.fsx"
//open Octokit

//Target "Release" (fun _ ->
//    StageAll ""
//    Git.Commit.Commit "" (sprintf "Bump version to %s" release.NugetVersion)
//    Branches.push ""

//    Branches.tag "" release.NugetVersion
//    Branches.pushTag "" "origin" release.NugetVersion
    
//    // release on github
//    createClient (getBuildParamOrDefault "github-user" "") (getBuildParamOrDefault "github-pw" "")
//    |> createDraft gitOwner gitName release.NugetVersion (release.SemVer.PreRelease <> None) release.Notes 
//    |> releaseDraft
//    |> Async.RunSynchronously
//)

Target "BuildPackage" DoNothing

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target "All" DoNothing

"Clean"
  ==> "AssemblyInfo"
  ==> "Build"
  ==> "RunTests"
  =?> ("GenerateReferenceDocs",isLocalBuild && not isMono)
  =?> ("GenerateDocs",isLocalBuild && not isMono)
  ==> "All"
  =?> ("ReleaseDocs",isLocalBuild && not isMono)

"All" 
//#if MONO
//#else
//  =?> ("SourceLink", Pdbstr.tryFind().IsSome )
//#endif
  ==> "NuGet"
  ==> "BuildPackage"

"CleanDocs"
  ==> "GenerateHelp"
  ==> "GenerateReferenceDocs"
  ==> "GenerateSlides"
  ==> "GenerateDocs"

"GenerateHelp"
  ==> "KeepRunning"
    
//"ReleaseDocs"
//  ==> "Release"

//"BuildPackage"
//  ==> "PublishNuGet"
//  ==> "Release"

RunTargetOrDefault "All"
