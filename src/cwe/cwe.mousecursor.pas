unit CWE.MouseCursor;

interface

uses
	Classes, Types, CWE.Core, TextMode, Graphics32;

const
	CURSOR_SYSTEM = 0;
	CURSOR_CUSTOM = 1;
	CURSOR_NONE   = 2;

type
	TMouseCursor = class
		Show,
		InWindow: 	Boolean;
		Bitmap,
		Background:	TBitmap32;
		Size,
		HotSpot,
		Scaling,
		Pos: 		TPoint;

		procedure	Draw;
		procedure	Erase;
		procedure 	SetImage(const Filename: String);
		constructor	Create(const Filename: String);
		destructor	Destroy; override;
	end;

var
	MouseCursor: TMouseCursor;


implementation


constructor TMouseCursor.Create(const Filename: String);
begin
	inherited Create;

	Bitmap := TBitmap32.Create;
	Background := TBitmap32.Create;

	SetImage(Filename);

	HotSpot.X := 0;
	HotSpot.Y := 0;

	Pos := Types.Point(-1, -1);
	InWindow := False;
	Show := False;
end;

destructor TMouseCursor.Destroy;
begin
	Bitmap.Free;
	Background.Free;

	inherited;
end;

procedure TMouseCursor.SetImage(const Filename: String);
begin
	LoadImage(Filename, Bitmap);

	Size.X := Bitmap.Width;
	Size.Y := Bitmap.Height;

	Background.SetSize(Size.X, Size.Y);
end;

procedure TMouseCursor.Draw;
begin
	if (Show) and (InWindow) and (Pos.X >= 0) then
	begin
		Background.Draw(0, 0,
			Bounds(Pos.X - HotSpot.X, Pos.Y - HotSpot.Y, Size.X, Size.Y),
			Console.Bitmap);
		if (Pos.X >= 0) and (Pos.Y >= 0) then
			Console.Bitmap.DrawColorKey(Pos.X - HotSpot.X, Pos.Y - HotSpot.Y,
				$FFFF00FF, Bitmap);
	end;
end;

procedure TMouseCursor.Erase;
begin
	if (Show) and (InWindow) and (Pos.X >= 0) then
		Console.Bitmap.Draw(Pos.X - HotSpot.X, Pos.Y - HotSpot.Y, Background);
end;

end.
