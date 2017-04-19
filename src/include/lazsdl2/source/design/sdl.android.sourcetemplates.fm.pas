unit SDL.Android.SourceTemplates.Fm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterJava, Forms, Controls,
  ExtCtrls, StdCtrls, ComCtrls, ActnList, Dialogs, IDEOptionsIntf;

type

  { TfrmSources }

  TfrmSources = class(TAbstractIDEOptionsEditor)
    acSources: TActionList;
    acInsert: TAction;
    acDelete: TAction;
    acSave: TAction;
    cmbTemplates: TComboBox;
    edtSource: TSynEdit;
    imgSources: TImageList;
    lblSources: TLabel;
    lblTemplates: TLabel;
    lsSources: TListBox;
    dlgSource: TOpenDialog;
    pnlTemplates: TPanel;
    SynJavaSyn1: TSynJavaSyn;
    tbSources: TToolBar;
    btnAddSource: TToolButton;
    btnDeleteSource: TToolButton;
    btnSaveSource: TToolButton;
    ToolButton4: TToolButton;
    procedure acDeleteExecute(Sender: TObject);
    procedure acDeleteUpdate(Sender: TObject);
    procedure acInsertExecute(Sender: TObject);
    procedure acInsertUpdate(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure acSaveUpdate(Sender: TObject);
    procedure cmbTemplatesSelect(Sender: TObject);
    procedure lsSourcesSelectionChange(Sender: TObject; User: boolean);
  private
    function GetSourceName: String;
    function GetTemplate: String;
  public
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;

    property Template: String read GetTemplate;
    property SourceName: String read GetSourceName;
  end;

implementation

{$R *.lfm}

uses
  SDL.Android.Options.Enviorment;

{ TfrmSources }

procedure TfrmSources.cmbTemplatesSelect(Sender: TObject);
begin
  with Sender as TComboBox do
    TJavaSources(Tag).FillSources(Text, lsSources.Items)
end;

procedure TfrmSources.acInsertExecute(Sender: TObject);
begin
  if dlgSource.Execute then
  begin
    TJavaSources(cmbTemplates.Tag).AddSource(cmbTemplates.Text, dlgSource.FileName)
  end
end;

procedure TfrmSources.acInsertUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := cmbTemplates.ItemIndex > -1
end;

procedure TfrmSources.acSaveExecute(Sender: TObject);
var
  JavaSrc: TJavaSources;
  Source: TStringList;
begin
  JavaSrc := TJavaSources(cmbTemplates.Tag);
  JavaSrc.UpdateSource(Template, SourceName, edtSource.Lines);
  if JavaSrc.ReadSource(Template, SourceName, Source) then
    edtSource.Lines.Assign(Source)
  else
    raise Exception.Create('Failed to save source!')
end;

procedure TfrmSources.acSaveUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := edtSource.Tag > 0
end;

procedure TfrmSources.acDeleteExecute(Sender: TObject);
begin
  TJavaSources(cmbTemplates.Tag).DeleteSource(cmbTemplates.Text, lsSources.Items[lsSources.ItemIndex])
end;

procedure TfrmSources.acDeleteUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := lsSources.ItemIndex > -1
end;

procedure TfrmSources.lsSourcesSelectionChange(Sender: TObject; User: boolean);
var
  Source: TSourceEntry;
begin
  with Sender as TListBox do
  begin
    if ItemIndex > -1 then
    begin
      Source := Items.Objects[ItemIndex] as TSourceEntry;
      edtSource.Tag := PtrInt(Source);
      edtSource.Lines.Assign(Source.Source)
    end
    else
    begin
      edtSource.Tag := 0;
      edtSource.Clear
    end
  end
end;

function TfrmSources.GetSourceName: String;
begin
  if lsSources.ItemIndex > -1 then
    Result := lsSources.Items[lsSources.ItemIndex]
  else
    Result := ''
end;

function TfrmSources.GetTemplate: String;
begin
  Result := cmbTemplates.Text
end;

function TfrmSources.GetTitle: String;
begin
  Result := 'Source Templates'
end;

procedure TfrmSources.Setup(ADialog: TAbstractOptionsEditorDialog);
begin

end;

procedure TfrmSources.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  if AOptions is TAndroidEnviormentOptions then with AOptions as TAndroidEnviormentOptions do
  begin
    cmbTemplates.Tag := PtrInt(Sources);
    Sources.Load;
    Sources.FillKeys(cmbTemplates.Items);
    Sources.FillSources(cmbTemplates.Text, lsSources.Items)
  end
end;

procedure TfrmSources.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  if AOptions is TAndroidEnviormentOptions then with AOptions as TAndroidEnviormentOptions do
  begin
    SaveOptions
  end
end;

class function TfrmSources.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TAndroidEnviormentOptions
end;

end.

