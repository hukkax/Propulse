unit CWE.Widgets.Scrollers;

interface

uses
	Classes, Types, SysUtils,
	CWE.Core;

type
	TCWEScrollbar = class;

	TCWEScrollEvent	= function (Sender: TCWEControl; NewOffset: Cardinal): Boolean of Object;

	TCWEScrollableControl = class(TCWEControl)
	private
	//	FOffset:	Cardinal;
		FOnScroll:	TCWEScrollEvent;
	public
		Offset:		Cardinal;
		MaxScroll:	Cardinal;
		Scrollbar:	TCWEScrollbar;

		procedure	Paint; override;
		procedure 	SetBounds(const Bounds: TRect); override;

		function 	AdjustedRect: TRect;
		procedure	CreateScrollbar;

		procedure	ScrollBy(Amount: Integer); virtual;
		procedure	ScrollTo(NewOffset: Cardinal);
		//procedure	SetOffset(const NewOffset: Cardinal);
		//property	Offset: Cardinal read FOffset write SetOffset;

		property	OnScroll: TCWEScrollEvent read FOnScroll write FOnScroll;
	end;

	TCWEScrollbar = class(TCWEControl)
	const
		SB_SCROLLDELAY = 25;	// delay until starting to repeat scrolling
		SB_SCROLLSPEED = 8;		// scrolling speed using scroll arrows

		SB_RECT_SCROLLBAR    = 0;
		SB_RECT_GUTTER       = 1;
		SB_RECT_THUMB        = 2;
		SB_RECT_BUTTON_UP    = 3;
		SB_RECT_BUTTON_DOWN  = 4;

		BUTTON_U = #30;//#24;
		BUTTON_D = #31;//#25;
		BUTTON_L = #27;
		BUTTON_R = #26;
	class var
		COLOR_SB_GUTTER,
		COLOR_SB_THUMB,
		COLOR_SB_BUTTON,
		COLOR_SB_ICON,
		COLOR_SB_HOVER,
		COLOR_SB_ACTIVE:	Byte;
	private
		Rects:		array[0..4] of TRect;
		HoveredZone,
		PrevHoveredZone: ShortInt;
		CapturePos: TPoint;
	public
		ItemsVisible,
		ItemCount:	Cardinal;
		Horizontal,
		Visible:	Boolean;

		Parent:		TCWEScrollableControl;

		constructor	Create(Owner: TCWEControl;
					const sCaption, sID: AnsiString; const Bounds: TRect;
					SetProtect: Boolean = False); override;

		procedure	AdjustBounds;
		procedure	Adjust(Item_Count, Items_Visible: Integer; Repaint: Boolean = False);
		procedure	Paint; override;

		function	MouseDown(Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean; override;
		function	MouseUp(Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean; override;
		function	MouseWheel(Shift: TShiftState; WheelDelta: Integer; P: TPoint): Boolean; override;
		procedure 	MouseMove(X, Y: Integer; P: TPoint); override;
		function	MouseLeave: Boolean; override;

		procedure 	ScrollCallback(Ctrl: TCWEControl; ID: Integer);
	end;


implementation

uses
	Math,
	CWE.MouseCursor;

// ==========================================================================
{ TCWEScrollableControl }
// ==========================================================================

function TCWEScrollableControl.AdjustedRect: TRect;
begin
	Result := Types.Rect(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
	if (Scrollbar <> nil) and (Scrollbar.Visible) then
	begin
		if Scrollbar.Horizontal then
			Result.Bottom := Result.Bottom + 1
		else
			Result.Right := Result.Right + 1;
	end;
end;

procedure TCWEScrollableControl.CreateScrollbar;
begin
	if Scrollbar <> nil then Scrollbar.Free;
	Scrollbar := TCWEScrollbar.Create(Self.Screen, '', ID + '_Scrollbar', Rect);
	Scrollbar.Parent := Self;
	Scrollbar.AdjustBounds;
	Scrollbar.Visible := True;
end;

procedure TCWEScrollableControl.Paint;
var
	Ctrl: TCWEControl;
begin
	if (Screen = nil) or (not Screen.Active) then Exit;
	if Border.Enabled then
		inherited DrawBorder(AdjustedRect, ColorBack);
	// Paint all owned controls
	if (Controls <> nil) and (Controls.Count > 0) then
		for Ctrl in Controls do
			Ctrl.Paint;
	if Scrollbar <> nil then
		Scrollbar.Paint;
end;

procedure TCWEScrollableControl.ScrollBy(Amount: Integer);
var
	N: Integer;
begin
	N := Offset + Amount;

	if Amount < 0 then
	begin
		if N < 0 then N := 0;
	end
	else
	begin
		if Offset = MaxScroll then Exit;
		if N > MaxScroll then N := MaxScroll;
	end;

	Offset := N;
	Paint;
end;

procedure TCWEScrollableControl.ScrollTo(NewOffset: Cardinal);
begin
	if (Offset = NewOffset) then Exit;

	if NewOffset > MaxScroll then NewOffset := MaxScroll;
	if (not Assigned(FOnScroll)) or (not FOnScroll(Self, NewOffset)) then
	begin
		Offset := NewOffset;
		Paint;
	end;
end;

procedure TCWEScrollableControl.SetBounds(const Bounds: TRect);
begin
	inherited;
	if Scrollbar <> nil then
		Scrollbar.AdjustBounds;
end;

{procedure TCWEScrollableControl.SetOffset(const NewOffset: Cardinal);
begin
	if NewOffset <> FOffset then
		ScrollTo(NewOffset);
end;}

// ==========================================================================
{ TCWEScrollbar }
// ==========================================================================

constructor TCWEScrollbar.Create;
begin
	inherited;
	Horizontal := False; //(Bounds.Right - Bounds.Left) > (Bounds.Bottom - Bounds.Top);
	WantMouse := True;
	WantHover := True;
	WantKeyboard := False;
	WantPixelPrecision := True;
end;

procedure TCWEScrollbar.MouseMove(X, Y: Integer; P: TPoint);
var
	i: Integer;
	R: TRect;
begin
	if not Visible then Exit;

	// user is currently moving the thumb
	if Capturing then
	begin
		// all content visible?
		if ItemsVisible < ItemCount then
		begin
			R := Rects[SB_RECT_GUTTER];

			if Horizontal then
				i := Trunc( (MouseCursor.Pos.X - R.Left - CapturePos.X)
					 / (R.Right - R.Left) * ItemCount )
			else
				i := Trunc( (MouseCursor.Pos.Y - R.Top - CapturePos.Y)
					 / (R.Bottom - R.Top) * ItemCount );

			Parent.ScrollTo(Max(i, 0));
			Paint;
		end;

		Exit;
	end;

	if Hovered then
	begin
		for i := 4 downto 0 do
		begin
			if PtInRect(Rects[i], MouseCursor.Pos) then
			begin
				if i = PrevHoveredZone then Exit; // prevent needless redraws
				DisableTimerCallback;
				HoveredZone := i;
				PrevHoveredZone := i;
				Paint;
				Exit;
				//if i <> SB_RECT_TRACK then Exit; // handled elsewhere
			end;
		end;
	end;

	if HoveredZone >= 0 then
	begin
		HoveredZone := -1;
		if PrevHoveredZone >= 0 then
			Paint;
		PrevHoveredZone := -1;
	end;
end;

function TCWEScrollbar.MouseLeave: Boolean;
begin
	Result := True;
	HoveredZone := -1;
	PrevHoveredZone := -1;
	DisableTimerCallback;
end;

procedure TCWEScrollbar.ScrollCallback(Ctrl: TCWEControl; ID: Integer);
begin
	case ID of

		SB_RECT_BUTTON_DOWN:
			Parent.ScrollBy(1);

		SB_RECT_BUTTON_UP:
			Parent.ScrollBy(-1);

	end;
	if Ctrl <> nil then
		TimerCallback.Interval := SB_SCROLLSPEED;
end;

function TCWEScrollbar.MouseDown(Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean;
begin
	if (not Visible) or (Button = mbMiddle) then Exit(False);

	Result := True;
	Screen.ActiveControl := Parent;
	DisableTimerCallback;

	case HoveredZone of

		SB_RECT_BUTTON_DOWN,
		SB_RECT_BUTTON_UP:
			case Button of

				mbLeft:
				begin
					ScrollCallback(nil, HoveredZone);
					SetTimerCallback(ScrollCallback, HoveredZone, SB_SCROLLDELAY);
				end;

				mbRight:
				begin
					if HoveredZone = SB_RECT_BUTTON_UP then
						Parent.ScrollTo(0)
					else
						Parent.ScrollTo(Parent.MaxScroll);
				end;
			end;

		SB_RECT_THUMB:
		begin
			Screen.MouseInfo.Control := Self;
			Screen.MouseInfo.Capturing := True;
			Paint;
			CapturePos := Types.Point(
				MouseCursor.Pos.X - Rects[SB_RECT_THUMB].Left,
				MouseCursor.Pos.Y - Rects[SB_RECT_THUMB].Top);
		end;

		SB_RECT_GUTTER:	// page up/down by clicking on gutter
			if Horizontal then
			begin
				if MouseCursor.Pos.X < Rects[SB_RECT_THUMB].Left then
					Parent.ScrollBy(-ItemsVisible)
				else
				if MouseCursor.Pos.X >= Rects[SB_RECT_THUMB].Right then
					Parent.ScrollBy(+ItemsVisible);
			end
			else
			begin
				if MouseCursor.Pos.Y < Rects[SB_RECT_THUMB].Top then
					Parent.ScrollBy(-ItemsVisible)
				else
				if MouseCursor.Pos.Y >= Rects[SB_RECT_THUMB].Bottom then
					Parent.ScrollBy(+ItemsVisible);
			end;

	end;
end;

function TCWEScrollbar.MouseUp(Button: TMouseButton; X, Y: Integer; P: TPoint): Boolean;
begin
	if (not Visible) then Exit(False);

	DisableTimerCallback;

	if Hovered then
		MouseMove(X, Y, P)
	else
	begin
		HoveredZone := -1;
		PrevHoveredZone := -1;
		Screen.ActivateControl(Parent);
	end;

	Paint;
	Result := (Button <> mbMiddle);
end;

function TCWEScrollbar.MouseWheel(Shift: TShiftState; WheelDelta: Integer; P: TPoint): Boolean;
begin
	if not Screen.MouseInfo.Capturing then
		Result := Parent.MouseWheel(Shift, WheelDelta, P)
	else
		Result := False;
end;

procedure TCWEScrollbar.Adjust(Item_Count, Items_Visible: Integer; Repaint: Boolean = False);
begin
	ItemCount := Item_Count;
	ItemsVisible := Items_Visible;
	if Repaint then Paint;
end;

procedure TCWEScrollbar.AdjustBounds;
var
	PR: TRect;
	W, H: Integer;
begin
	PR := Parent.Rect;
	OffsetRect(PR, -(Screen.Rect.Left), -(Screen.Rect.Top));

	if Horizontal then
	begin
		PR.Top := PR.Bottom;
		PR.Bottom := PR.Bottom + 1;
	end
	else
	begin
		PR.Left := PR.Right;
		PR.Right := PR.Right + 1;
	end;
	SetBounds(PR);

	W := Console.Font.Width;
	H := Console.Font.Height;

	Rects[SB_RECT_BUTTON_UP]   := Bounds(PR.Left*W, PR.Top*H, W, H);
	if Horizontal then
	begin
		Rects[SB_RECT_SCROLLBAR]   := Types.Rect(PR.Left*W, PR.Top*H, (PR.Right-1)*W, PR.Bottom*H);
		Rects[SB_RECT_GUTTER]      := Types.Rect((PR.Left+1)*W, PR.Top*H, (PR.Right-1)*W, PR.Bottom*H);
		Rects[SB_RECT_BUTTON_DOWN] := Bounds((PR.Right-1)*W, PR.Top*H, W, H);
	end
	else
	begin
		Rects[SB_RECT_SCROLLBAR]   := Types.Rect(PR.Left*W, PR.Top*H, PR.Right*W, (PR.Bottom-1)*H);
		Rects[SB_RECT_GUTTER]      := Types.Rect(PR.Left*W, (PR.Top+1)*H, PR.Right*W, (PR.Bottom-1)*H);
		Rects[SB_RECT_BUTTON_DOWN] := Bounds(PR.Left*W, (PR.Bottom-1)*H, W, H);
	end;
end;

procedure TCWEScrollbar.Paint;
var
	H, GH, X1, X2, Y1, Y2: Integer;
	IH: Single;
	R: TRect;
begin
	if not Visible then Exit;

	// are the control's dimensions sane?
	if Horizontal then
	begin
		X1 := Rect.Left;
		X2 := Rect.Right-1;
		if X2 - X1 < 3 then Exit;
		Inc(X1); Dec(X2);
		if (X2 <= X1) then Exit;	// no room for thumb
	end
	else
	begin
		Y1 := Rect.Top;
		Y2 := Rect.Bottom-1;
		if Y2 - Y1 < 3 then Exit;
		Inc(Y1); Dec(Y2);
		if (Y2 <= Y1) then Exit;	// no room for thumb
	end;

	Console.FillRect(Rect, ' ', COLOR_SB_ICON, COLOR_SB_GUTTER);

	GetPixelRect(R);

	if Horizontal then
	begin
		X1 := R.Left  + Console.Font.Width;
		X2 := R.Right - Console.Font.Width + 0;
		Y1 := R.Top;
		Y2 := R.Bottom;
		GH := X2 - X1;						// pixel width of the gutter area
	end
	else
	begin
		X1 := R.Left;
		X2 := R.Right;
		Y1 := R.Top    + Console.Font.Height;
		Y2 := R.Bottom - Console.Font.Height + 1;
		GH := Y2 - Y1; 						// pixel height of the gutter area
	end;

	// all content visible?
	if ItemsVisible < ItemCount then
	begin
		IH := GH / ItemCount;				// pixel height of one item
		H  := Round(IH * ItemsVisible);		// scale by visible items
		if H < 6 then H := 6;				// minimum size of thumb
		GH := Round(IH * Parent.Offset);

		if Horizontal then
		begin
			X2 := Min(X1 + H + GH, X2);
			Inc(X1, GH);
		end
		else
		begin
			Y2 := Min(Y1 + H + GH, Y2);
			Inc(Y1, GH);
		end;

		Rects[SB_RECT_THUMB] := Types.Rect(X1, Y1, X2, Y2);
		if Capturing then
			H := COLOR_SB_ACTIVE
		else
		if PtInRect(Rects[SB_RECT_THUMB], MouseCursor.Pos) then
			H := COLOR_SB_HOVER
		else
			H := COLOR_SB_THUMB;
		Console.Bitmap.FillRectS(X1, Y1, X2, Y2, Console.Palette[H]);
	end
	else
		Rects[SB_RECT_THUMB] := Types.Rect(-1, -1, -1, -1);

	Y1 := Rect.Top;
	Y2 := Rect.Bottom-1;

	// Up button
	if Parent.Offset > 0 then
	begin
		if HoveredZone = SB_RECT_BUTTON_UP then
			H := COLOR_SB_HOVER
		else
			H := COLOR_SB_BUTTON;
	end
	else
		H := COLOR_SB_GUTTER;
	X1 := Rect.Left;
	if Horizontal then
		Console.Write(BUTTON_L, X1, Y1, COLOR_SB_ICON, H)
	else
		Console.Write(BUTTON_U, X1, Y1, COLOR_SB_ICON, H);

	// Down button
	if (ItemCount > ItemsVisible) and (Parent.Offset < Parent.MaxScroll) then
	begin
		if HoveredZone = SB_RECT_BUTTON_DOWN then
			H := COLOR_SB_HOVER
		else
			H := COLOR_SB_BUTTON;
	end
	else
		H := COLOR_SB_GUTTER;
	if Horizontal then
		Console.Write(BUTTON_R, Rect.Right-1, Y1, COLOR_SB_ICON, H)
	else
		Console.Write(BUTTON_D, X1, Y2, COLOR_SB_ICON, H);
end;


end.





