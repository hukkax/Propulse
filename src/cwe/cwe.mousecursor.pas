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
	private
		function 	ValidCoords(var X, Y: Integer): Boolean;
	public
		Show,
		InWindow: 	Boolean;
		Bitmap,
		Background:	TBitmap32;
		Size,
		HotSpot,
		Scaling,
		OldPos,
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

function TMouseCursor.ValidCoords(var X, Y: Integer): Boolean;
begin
	X := Pos.X - HotSpot.X;
	Y := Pos.Y - HotSpot.Y;
	Result := (Show) and (InWindow) and
		(X >= 0) and (Y >= 0) and
		(X < Console.Bitmap.Width) and (Y < Console.Bitmap.Height);
end;

procedure TMouseCursor.Draw;
var
	X, Y: Integer;
begin
	if ValidCoords(X, Y) then
	begin
		Background.Draw(0, 0, Bounds(X, Y, Size.X, Size.Y), Console.Bitmap);
		Console.Bitmap.DrawColorKey(X, Y, $FFFF00FF, Bitmap);
	end;
end;

procedure TMouseCursor.Erase;
var
	X, Y: Integer;
begin
	if ValidCoords(X, Y) then
		Console.Bitmap.Draw(X, Y, Background);
end;

end.
