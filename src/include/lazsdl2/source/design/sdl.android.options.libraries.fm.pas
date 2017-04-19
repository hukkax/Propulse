unit SDL.Android.Options.Libraries.Fm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ComCtrls, ActnList,
  Dialogs, IDEOptionsIntf, SDL.Android.Options.Project;

type

  { TfrmAndroidLibraries }

  TfrmAndroidLibraries = class(TAbstractIDEOptionsEditor)
    acAddLibs: TAction;
    acDeleteLibs: TAction;
    acLibs: TActionList;
    acReplaceLib: TAction;
    dlgLibs: TOpenDialog;
    imgLibs: TImageList;
    mmLibs: TMemo;
    StatusBar1: TStatusBar;
    tlbLibs: TToolBar;
    btnAddLib: TToolButton;
    btnDeleteLib: TToolButton;
    btnReplaceLib: TToolButton;
    procedure acAddLibsExecute(Sender: TObject);
    procedure acDeleteLibsExecute(Sender: TObject);
    procedure acReplaceLibExecute(Sender: TObject);
    procedure LibrarySelected(Sender: TObject);
    procedure UpdateStatusBar(Sender: TObject);
    procedure mmLibsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
  private
    function InternalRelativePath(const Path: String): String;
  public
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

uses
  LazIDEIntf, LazFileUtils, MacroIntf;

{ TfrmAndroidLibraries }

procedure TfrmAndroidLibraries.acAddLibsExecute(Sender: TObject);
var
  Lib: String;
begin
  dlgLibs.Options := dlgLibs.Options + [ofAllowMultiSelect];
  if dlgLibs.Execute then
  begin
    for Lib in dlgLibs.Files do
    begin
      mmLibs.Lines.Add(InternalRelativePath(Lib))
    end
  end
end;

procedure TfrmAndroidLibraries.acDeleteLibsExecute(Sender: TObject);
begin
  mmLibs.Lines.Delete(mmLibs.CaretPos.Y)
end;

procedure TfrmAndroidLibraries.acReplaceLibExecute(Sender: TObject);
begin
  dlgLibs.Options := dlgLibs.Options - [ofAllowMultiSelect];
  if dlgLibs.Execute then
    mmLibs.Lines[mmLibs.CaretPos.Y] := InternalRelativePath(dlgLibs.FileName)
end;

procedure TfrmAndroidLibraries.LibrarySelected(Sender: TObject);
begin
  TAction(Sender).Enabled := (mmLibs.CaretPos.Y >= 0) and (mmLibs.Lines.Count > 0)
end;

procedure TfrmAndroidLibraries.UpdateStatusBar(Sender: TObject);
var
  Macro: String;
begin
  if mmLibs.CaretPos.Y > -1 then
  begin
    Macro := mmLibs.Lines[mmLibs.CaretPos.Y];
    if IDEMacros.SubstituteMacros(Macro) then
      StatusBar1.Panels[0].Text := Macro
    else
      StatusBar1.Panels[0].Text := 'Macro error'
  end
  else
    StatusBar1.Panels[0].Text := IntToStr(mmLibs.CaretPos.Y);
end;

procedure TfrmAndroidLibraries.mmLibsMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin

end;

function TfrmAndroidLibraries.InternalRelativePath(const Path: String): String;
begin
  Result := ExtractRelativepath(AppendPathDelim(ExtractFilePath(LazarusIDE.ActiveProject.MainFile.GetFullFilename)), Path);
end;

function TfrmAndroidLibraries.GetTitle: String;
begin
  Result := 'Libraries'
end;

procedure TfrmAndroidLibraries.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
end;

procedure TfrmAndroidLibraries.ReadSettings(AOptions: TAbstractIDEOptions);
var
  Lib: String;
begin
  if AOptions is TAndroidProjectOptions then with AOptions as TAndroidProjectOptions do
  begin
    mmLibs.Clear;
    for Lib in AndroidLibs do
      mmLibs.Lines.Add(Lib)
  end
end;

procedure TfrmAndroidLibraries.WriteSettings(AOptions: TAbstractIDEOptions);
var
  Lib: String;
begin
  if AOptions is TAndroidProjectOptions then with AOptions as TAndroidProjectOptions do
  begin
    AndroidLibs.Clear;
    for Lib in mmLibs.Lines do
      AndroidLibs.Add(Lib);
    SaveOptions
  end
end;

class function TfrmAndroidLibraries.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TAndroidProjectOptions
end;

end.

