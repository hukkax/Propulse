unit MainFM;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ExtDlgs, SDL.Extended.LibraryProvider;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnLoad: TBitBtn;
    Image1: TImage;
    OpenPictureDialog1: TOpenPictureDialog;
    pnlButton: TPanel;
    SDLLibraryProvider1: TSDLLibraryProvider;
    procedure btnLoadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    procedure LoadImage(const FileName: String);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

uses
  SDL.Extended.Utils, GraphType, SDL.Api.libSDL_Image;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  LoadImage('data/sdl.bmp')
end;

procedure TfrmMain.btnLoadClick(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
    LoadImage(OpenPictureDialog1.FileName);
end;

procedure TfrmMain.LoadImage(const FileName: String);
var
  RawImage: TRawImage;
  Bitmap: TBitmap;
begin
  if LoadSDLImage(FileName, SDLLibraryProvider1.Libs['SDL_Image'] as TSDL2_ImageLibrary, RawImage) then
  begin
    Bitmap := TBitmap.Create;
    try
      Bitmap.LoadFromRawImage(RawImage, True);
      Image1.Picture.Bitmap.Assign(Bitmap);
    finally
      Bitmap.Free;
    end;
  end;
end;

end.

