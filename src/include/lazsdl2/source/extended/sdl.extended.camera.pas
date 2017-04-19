unit SDL.Extended.Camera;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  SDL.Extended.LibObject;

type

  TSDLCameraUpVector = (upNone, upX, upY, upZ);

  { TSDLCustomCamera }

  TSDLCustomCamera = class(TSDLObject)
  public type

    PSDLCameraData = ^TSDLCameraData;
    TSDLCameraData = record
      eyeX: Double;
      eyeY: Double;
      eyeZ: Double;
      centerX: Double;
      centerY: Double;
      centerZ: Double;
      upX: Double;
      upY: Double;
      upZ: Double;
    end;

    { TSDLCameraCenter }

    TSDLCameraCenter = class(TPersistent)
    private
      FOwner: TSDLCustomCamera;
      function GetX: Double;
      function GetY: Double;
      function GetZ: Double;
      procedure SetX(AValue: Double);
      procedure SetY(AValue: Double);
      procedure SetZ(AValue: Double);
    public
      constructor Create(Owner: TSDLCustomCamera); reintroduce;
    published
      property X: Double read GetX write SetX;
      property Y: Double read GetY write SetY;
      property Z: Double read GetZ write SetZ;
    end;

    { TSDLCameraEye }

    TSDLCameraEye = class(TPersistent)
    private
      FOwner: TSDLCustomCamera;
      function GetX: Double;
      function GetY: Double;
      function GetZ: Double;
      procedure SetX(AValue: Double);
      procedure SetY(AValue: Double);
      procedure SetZ(AValue: Double);
    public
      constructor Create(Owner: TSDLCustomCamera); reintroduce;
    published
      property X: Double read GetX write SetX;
      property Y: Double read GetY write SetY;
      property Z: Double read GetZ write SetZ;
    end;

  private
    FActive: Boolean;
    FCenter: TSDLCameraCenter;
    FData: Pointer;
    FEye: TSDLCameraEye;
    function GetUp: TSDLCameraUpVector;
    procedure SetActive(AValue: Boolean);
    procedure SetUp(AValue: TSDLCameraUpVector);
  protected
    procedure DoReload; override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property Data: Pointer read FData; (** OpenGL specifics data structure *)
    property Active: Boolean read FActive write SetActive; (** Activate/deactivate camera *)
    property Center: TSDLCameraCenter read FCenter write FCenter; (** Specifies the position of the reference point. *)
    property Eye: TSDLCameraEye read FEye write FEye; (** Specifies the position of the eye point. *)
    property Up: TSDLCameraUpVector read GetUp write SetUp; (** Specifies the direction of the up vector *)
  end;

  TSDLCamera = class(TSDLCustomCamera)
  published
    property Active;
    property Center;
    property Eye;
    property Provider;
    property Up;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I sdl.extended.camera_icon.lrs}
  RegisterComponents('SDL',[TSDLCamera]);
end;

{ TSDLCustomCamera.TSDLCameraCenter }

function TSDLCustomCamera.TSDLCameraCenter.GetX: Double;
begin
  Result := PSDLCameraData(FOwner.Data)^.centerX
end;

function TSDLCustomCamera.TSDLCameraCenter.GetY: Double;
begin
  Result := PSDLCameraData(FOwner.Data)^.centerY
end;

function TSDLCustomCamera.TSDLCameraCenter.GetZ: Double;
begin
  Result := PSDLCameraData(FOwner.Data)^.centerZ
end;

procedure TSDLCustomCamera.TSDLCameraCenter.SetX(AValue: Double);
begin
  PSDLCameraData(FOwner.Data)^.centerX := AValue
end;

procedure TSDLCustomCamera.TSDLCameraCenter.SetY(AValue: Double);
begin
  PSDLCameraData(FOwner.Data)^.centerY := AValue
end;

procedure TSDLCustomCamera.TSDLCameraCenter.SetZ(AValue: Double);
begin
  PSDLCameraData(FOwner.Data)^.centerZ := AValue
end;

constructor TSDLCustomCamera.TSDLCameraCenter.Create(Owner: TSDLCustomCamera);
begin
  FOwner := Owner
end;

{ TSDLCustomCamera.TSDLCameraEye }

function TSDLCustomCamera.TSDLCameraEye.GetX: Double;
begin
  Result := PSDLCameraData(FOwner.Data)^.eyeX
end;

function TSDLCustomCamera.TSDLCameraEye.GetY: Double;
begin
  Result := PSDLCameraData(FOwner.Data)^.eyeY
end;

function TSDLCustomCamera.TSDLCameraEye.GetZ: Double;
begin
  Result := PSDLCameraData(FOwner.Data)^.eyeZ
end;

procedure TSDLCustomCamera.TSDLCameraEye.SetX(AValue: Double);
begin
  PSDLCameraData(FOwner.Data)^.eyeX := AValue
end;

procedure TSDLCustomCamera.TSDLCameraEye.SetY(AValue: Double);
begin
  PSDLCameraData(FOwner.Data)^.eyeY := AValue
end;

procedure TSDLCustomCamera.TSDLCameraEye.SetZ(AValue: Double);
begin
  PSDLCameraData(FOwner.Data)^.eyeZ := AValue
end;

constructor TSDLCustomCamera.TSDLCameraEye.Create(Owner: TSDLCustomCamera);
begin
  FOwner := Owner;
end;

{ TSDLCustomCamera }

procedure TSDLCustomCamera.SetActive(AValue: Boolean);
begin
  if FActive=AValue then Exit;
  FActive:=AValue;
end;

procedure TSDLCustomCamera.SetUp(AValue: TSDLCameraUpVector);
begin
  PSDLCameraData(FData)^.upX := 0;
  PSDLCameraData(FData)^.upY := 0;
  PSDLCameraData(FData)^.upZ := 0;

  case AValue of
    upX: PSDLCameraData(FData)^.upX := 1;
    upY: PSDLCameraData(FData)^.upY := 1;
    upZ: PSDLCameraData(FData)^.upZ := 1;
  end;
end;

procedure TSDLCustomCamera.DoReload;
begin

end;

function TSDLCustomCamera.GetUp: TSDLCameraUpVector;
begin
  if PSDLCameraData(FData)^.upY = 1 then
    Result := upY
  else
  if PSDLCameraData(FData)^.upZ = 1 then
    Result := upZ
  else
  if PSDLCameraData(FData)^.upX = 1 then
    Result := upX
  else
    Result := upNone
end;

procedure TSDLCustomCamera.AfterConstruction;
begin
  inherited AfterConstruction;

  FData := GetMem(SizeOf(TSDLCameraData));
  FillByte(FData^, SizeOf(TSDLCameraData), 0);

  FCenter := TSDLCameraCenter.Create(Self);
  FEye := TSDLCameraEye.Create(Self);
end;

procedure TSDLCustomCamera.BeforeDestruction;
begin
  Freemem(FData);
  FCenter.Free;
  FEye.Free;

  inherited BeforeDestruction;
end;

end.
