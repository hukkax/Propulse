unit SDL.Android.Options.Enviorment.Fm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, ValEdit, ExtCtrls, ActnList, IDEOptionsIntf, SDL.Android.Options.Enviorment;

type

  { TfrmAndroidOptions }

  TfrmAndroidOptions = class(TAbstractIDEOptionsEditor)
    acAndroid: TActionList;
    acUpdateSDK: TAction;
    Bevel1: TBevel;
    btnAndroidSDK: TButton;
    cmbTargetSDK: TComboBox;
    cmbTargetMinSDK: TComboBox;
    edAndroidNDK: TDirectoryEdit;
    edAndroidSDK: TDirectoryEdit;
    edtAnt: TFileNameEdit;
    lblAnt: TLabel;
    lblMinAndroidVersion: TLabel;
    lblAndroidVersion: TLabel;
    lblTarget: TLabel;
    lblAndroidNDK: TLabel;
    lblAndroidSDK: TLabel;
    lblMacros: TLabel;
    lblTargetMinSDK: TLabel;
    pnlData: TPanel;
    pnlMacros: TPanel;
    Splitter1: TSplitter;
    valMacros: TValueListEditor;
    procedure acUpdateSDKExecute(Sender: TObject);
    procedure acUpdateSDKUpdate(Sender: TObject);
    procedure cmbTargetMinSDKChange(Sender: TObject);
    procedure cmbTargetMinSDKSelect(Sender: TObject);
    procedure cmbTargetSDKChange(Sender: TObject);
    procedure cmbTargetSDKSelect(Sender: TObject);
    procedure edAndroidNDKChange(Sender: TObject);
    procedure edAndroidSDKChange(Sender: TObject);
  private
    procedure InternalUpdateMacros;
    procedure InternalFillTargets;
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
  process, MacroIntf, LazFileUtils;

{ TfrmAndroidOptions }

procedure TfrmAndroidOptions.edAndroidNDKChange(Sender: TObject);
begin
  TAndroidEnviormentOptions(TAndroidEnviormentOptions.GetInstance).AndroidNDK := TDirectoryEdit(Sender).Directory;
  InternalFillTargets;
  InternalUpdateMacros;
end;

procedure TfrmAndroidOptions.cmbTargetSDKChange(Sender: TObject);
begin
  TAndroidEnviormentOptions(TAndroidEnviormentOptions.GetInstance).TargetSDK := TComboBox(Sender).Text;
  InternalUpdateMacros;
end;

procedure TfrmAndroidOptions.cmbTargetSDKSelect(Sender: TObject);
var
  Options: TAndroidEnviormentOptions;
begin
  Options := TAndroidEnviormentOptions(TAndroidEnviormentOptions.GetInstance);
  lblAndroidVersion.Caption := '-' + Options.TargetVersion(cmbTargetSDK.Text)
end;

procedure TfrmAndroidOptions.acUpdateSDKUpdate(Sender: TObject);
begin
  if TAction(Sender).ActionComponent is TButton then
    TButton(TAction(Sender).ActionComponent).Enabled := TAndroidEnviormentOptions(TAndroidEnviormentOptions.GetInstance).AndroidSDK <> ''
end;

procedure TfrmAndroidOptions.cmbTargetMinSDKChange(Sender: TObject);
begin
  TAndroidEnviormentOptions(TAndroidEnviormentOptions.GetInstance).TargetMinSDK := TComboBox(Sender).Text;
  InternalUpdateMacros;
end;

procedure TfrmAndroidOptions.cmbTargetMinSDKSelect(Sender: TObject);
var
  Options: TAndroidEnviormentOptions;
begin
  Options := TAndroidEnviormentOptions(TAndroidEnviormentOptions.GetInstance);
  lblMinAndroidVersion.Caption := '-' + Options.TargetVersion(cmbTargetMinSDK.Text)
end;

procedure TfrmAndroidOptions.acUpdateSDKExecute(Sender: TObject);
var
  LazProcess: TProcess;
  Tools: String;
  Executable: String;
  Files: TStringList;
  i: Integer;
begin
  Tools := AppendPathDelim(TAndroidEnviormentOptions(TAndroidEnviormentOptions.GetInstance).AndroidSDK) + 'tools' + PathDelim;

  Executable := '';
  Files := FindAllFiles(Tools, '*', False);
  try
    for i := 0 to Files.Count - 1 do
    begin
      if Pos('android', ExtractRelativepath(Tools, Files[i])) = 1 then
      begin
        Executable := Files[i];
        Break;
      end;
    end;
  finally
    Files.Free;
  end;

  if Executable <> '' then
  begin
    LazProcess := TProcess.Create(nil);
    try
      LazProcess.Options := LazProcess.Options + [poWaitOnExit];
      //LazProcess.CurrentDirectory := ExtractFileDir(Executable);
      LazProcess.Executable := Executable;
      LazProcess.Execute;
    finally
      LazProcess.Free
    end
  end
  else
    MessageDlg('Could not found SDK manager executable', mtWarning, [mbOK], 0)
end;

procedure TfrmAndroidOptions.edAndroidSDKChange(Sender: TObject);
begin
  TAndroidEnviormentOptions(TAndroidEnviormentOptions.GetInstance).AndroidSDK := TDirectoryEdit(Sender).Directory;
  InternalUpdateMacros;
end;

procedure TfrmAndroidOptions.InternalUpdateMacros;
var
  Key: String;
  Macro: String;
  i: Integer;
begin
  for i := 1 to valMacros.RowCount - 1 do
  begin
    Key := valMacros.Keys[i];
    Macro := Key;

    if IDEMacros.SubstituteMacros(Macro) then
      valMacros.Values[Key] := Macro
    else
      valMacros.Values[Key] := ''
  end
end;

procedure TfrmAndroidOptions.InternalFillTargets;
var
  Options: TAndroidEnviormentOptions;
begin
  Options := TAndroidEnviormentOptions(TAndroidEnviormentOptions.GetInstance);
  Options.FillTargets(cmbTargetSDK.Items);
  Options.FillTargets(cmbTargetMinSDK.Items);
  cmbTargetSDK.ItemIndex := cmbTargetSDK.Items.IndexOf(Options.TargetSDK);
  cmbTargetMinSDK.ItemIndex := cmbTargetMinSDK.Items.IndexOf(Options.TargetMinSDK);
  cmbTargetSDK.OnSelect(nil);
  cmbTargetMinSDK.OnSelect(nil);
end;

function TfrmAndroidOptions.GetTitle: String;
begin
  Result := 'General';
end;

procedure TfrmAndroidOptions.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
end;

procedure TfrmAndroidOptions.ReadSettings(AOptions: TAbstractIDEOptions);
var
  Options: TAndroidEnviormentOptions;
begin
  if AOptions is TAndroidEnviormentOptions then
  begin
    Options := TAndroidEnviormentOptions(TAndroidEnviormentOptions.GetInstance);
    edAndroidSDK.Directory := Options.AndroidSDK;
    edAndroidNDK.Directory := Options.AndroidNDK;
    edtAnt.FileName := Options.Ant;
    InternalFillTargets;
    InternalUpdateMacros;
  end;
end;

procedure TfrmAndroidOptions.WriteSettings(AOptions: TAbstractIDEOptions);
var
  Options: TAndroidEnviormentOptions;
begin
  if AOptions is TAndroidEnviormentOptions then
  begin
    Options := TAndroidEnviormentOptions(TAndroidEnviormentOptions.GetInstance);
    Options.AndroidSDK := edAndroidSDK.Directory;
    Options.AndroidNDK := edAndroidNDK.Directory;
    Options.TargetSDK := cmbTargetSDK.Text;
    Options.Ant := edtAnt.FileName;
    Options.SaveOptions;
  end;
end;

class function TfrmAndroidOptions.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TAndroidEnviormentOptions
end;

end.

