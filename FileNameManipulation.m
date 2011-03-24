BeginPackage["FileNameManipulation`"];

FileNameMapAt::usage = "FileNameMapAt[f, \"name\", n] applies f to the nth path element in the file name \"name\". n can be a numerical position such as in FileNameTake, or a named position such as \"BaseName\", \"Extension\", or \"Directory\"."

Begin["Private`"]

FileNameMapAt[foo_, file_, pos_]:=
	Switch[pos,
	"BaseName",
		ToFileName[DirectoryName@file, foo[FileBaseName@file] <> (If[# == "", "", "." <> #] &[FileExtension@file])],
	"Extension",
	ToFileName[DirectoryName@file,FileBaseName@file <> (If[# == "", "", "." <>#] &[foo@FileExtension@file])],
	"Directory",
	ToFileName[foo[DirectoryName@file], FileNameTake@file],
	i_Integer /; i <= FileNameDepth@file,
	FileNameJoin@MapAt[foo,FileNameSplit[file],pos]
	]

(* These are still internal functions but they can be made available by declaring them above Begin["Private`"] *)

SelectByFileType[files_, type_]:=
	Select[files, FileExtension@# == type &]

FlattenDirectory[dir_] :=
 With[{dirs = Select[FileNames["*", dir], DirectoryQ]},
  Function[subdir,
    RenameFile[#,
       ToFileName[ParentDirectory@DirectoryName@#,
        FileNameTake@#]] & /@ FileNames["*", subdir]] /@ dirs]

End[]

EndPackage[]
