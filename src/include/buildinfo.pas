unit BuildInfo;

interface

const
	CompileDate = {$I %DATE%};
	CompileTime = {$I %TIME%};

var
	Build: record
		CompileDate: String;
		CompileTime: String;
	end;

implementation

uses
	SysUtils, StrUtils;

initialization

	Build.CompileDate := Trim(ReplaceStr(CompileDate, '/', '-'));
	Build.CompileTime := Trim(CompileTime);

end.

