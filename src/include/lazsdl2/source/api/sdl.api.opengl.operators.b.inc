 
{$REGION 'GL_ClearBufferTypes'}

operator in (const Left: GL_ClearBufferType; const Right: GL_ClearBufferTypes): Boolean; inline;
begin
  Result := Int32(Ord(Left) and Right.Value) = Ord(Left)
end;
 
operator or (const Left: GL_ClearBufferType; const Right: GL_ClearBufferType): GL_ClearBufferTypes; inline;
begin
  Result.Value := Ord(Left) or Ord(Right) 
end;
 
operator or (const Left: GL_ClearBufferType; const Right: GL_ClearBufferTypes): GL_ClearBufferTypes; inline;
begin
  Result.Value := UInt32(Ord(Left)) or UInt32(Ord(Right.Value))
end;
 
operator or (const Left: GL_ClearBufferTypes; const Right: GL_ClearBufferType): GL_ClearBufferTypes; inline;
begin
  Result.Value := UInt32(Ord(Left.Value)) or UInt32(Ord(Right)) 
end;
 
operator = (const Left: GL_ClearBufferTypes; const Right: GL_ClearBufferType): Boolean; inline;
begin
  Result := UInt32(Left.Value) = UInt32(Ord(Right)) 
end;
 
operator = (const Left: GL_ClearBufferType; const Right: GL_ClearBufferTypes): Boolean; inline;
begin
  Result := UInt32(Ord(Left)) = UInt32(Right.Value) 
end;
 
operator = (const Left: GL_ClearBufferTypes; const Right: GL_ClearBufferTypes): Boolean; inline;
begin
  Result := Left.Value = Right.Value 
end;
 
operator <> (const Left: GL_ClearBufferTypes; const Right: GL_ClearBufferType): Boolean; inline;
begin
  Result := UInt32(Left.Value) <> UInt32(Ord(Right)) 
end;
 
operator <> (const Left: GL_ClearBufferType; const Right: GL_ClearBufferTypes): Boolean; inline;
begin
  Result := UInt32(Ord(Left)) <> UInt32(Right.Value)
end;
 
operator <> (const Left: GL_ClearBufferTypes; const Right: GL_ClearBufferTypes): Boolean; inline;
begin
  Result := Left.Value <> Right.Value 
end;
 
operator + (const Left: GL_ClearBufferTypes; const Right: GL_ClearBufferType): GL_ClearBufferTypes; inline;
begin
  Result.Value := UInt32(Left.Value) or UInt32(Ord(Right)) 
end;
 
operator + (const Left: GL_ClearBufferTypes; const Right: GL_ClearBufferTypes): GL_ClearBufferTypes; inline;
begin
  Result.Value := Left.Value or Right.Value 
end;
 
operator - (const Left: GL_ClearBufferTypes; const Right: GL_ClearBufferType): GL_ClearBufferTypes; inline;
begin
  Result.Value := UInt32(Left.Value) xor UInt32(Ord(Right)) 
end;
 
operator := (const Value: GL_ClearBufferType): GL_ClearBufferTypes; inline;
begin
  Result.Value := Ord(Value) 
end;
 
operator := (const Value: GL_ClearBufferType): String; inline;
begin
  Result := Value.ToString 
end;
 
operator := (const Value: GL_ClearBufferTypes): String; inline;
begin
  Result := Value.ToString 
end;

{ GL_Color }

procedure GL_Color.Make(out Color: GL_Color; ar, ag, ab, aa: GLfloat);
begin
  Color.r := ar;
  Color.g := ag;
  Color.b := ab;
  Color.a := aa;
end;

procedure GL_Color.Make(out Color: GL_Color; ar, ag, ab, aa: GLbyte);
begin
  Color.r := ar / 255;
  Color.g := ag / 255;
  Color.b := ab / 255;
  Color.a := aa / 255;
end;

procedure GL_Color.Make(out Color: GL_Color; Clr: GLint; Alpha: GLbyte);
begin
  Make(Color, Clr and $000000ff / 255, (Clr shr 8) and $000000ff, (Clr shr 16) and $000000ff / 255, Alpha / 255);
end;

function GL_Color.Make(ar, ag, ab, aa: GLfloat): GL_Color;
begin
  Make(Result, ar, ag, ab, aa)
end;

function GL_Color.Make(ar, ag, ab, aa: GLbyte): GL_Color;
begin
  Make(Result, ar, ag, ab, aa)
end;

function GL_Color.Make(Clr: GLint; Alpha: GLbyte): GL_Color;
begin
  Make(Result, Clr, Alpha)
end;

{ TOpenGLVector }

function TOpenGLVector.GetItems(Index: Integer): GLfloat;
begin
  Result := Data[Index];
end;

procedure TOpenGLVector.SetItems(Index: Integer; AValue: GLfloat);
begin
  Data[Index] := AValue;
end;

procedure TOpenGLVector.Normalize;
var
  r: GLfloat;
begin
    r := sqrt(Self[0] * Self[0] + Self[1] * Self[1] + Self[2] * Self[2]);

    if r = 0.0 then
      Exit;

    Self[0] := Self[0] / r;
    Self[1] := Self[1] / r;
    Self[2] := Self[2] / r;
end;

procedure TOpenGLVector.cross(v2: TOpenGLVector; out result: TOpenGLVector);
begin
  result[0] := Data[1] * v2[2] - Data[2] * v2[1];
  result[1] := Data[2] * v2[0] - Data[0] * v2[2];
  result[2] := Data[0] * v2[1] - Data[1] * v2[0];
end;

{ TOpenGLMatrix }

function TOpenGLMatrix.GetItems(Y, X: Integer): GLfloat;
begin
  Result := Data[Y, X];
end;

procedure TOpenGLMatrix.SetItems(Y, X: Integer; AValue: GLfloat);
begin
  Data[Y, X] := AValue;
end;

procedure TOpenGLMatrix.glLoadIdentity;
begin
  Data[0,0] := 1;
  Data[0,1] := 0;
  Data[0,2] := 0;
  Data[0,3] := 0;

  Data[1,0] := 0;
  Data[1,1] := 1;
  Data[1,2] := 0;
  Data[1,3] := 0;

  Data[2,0] := 0;
  Data[2,1] := 0;
  Data[2,2] := 1;
  Data[2,3] := 0;

  Data[3,0] := 0;
  Data[3,1] := 0;
  Data[3,2] := 0;
  Data[3,3] := 1
end;

procedure TOpenGLMatrix.glMultMatrix(MatrixA: TOpenGLMatrix);
var
  NewMatrix: TOpenGLMatrixData;
  i: Integer;
begin
  for i := 0 to 3 do
  begin
    NewMatrix[i, 0] :=
      MatrixA[i, 0] * Data[0, 0] +
      MatrixA[i, 1] * Data[1, 0] +
      MatrixA[i, 2] * Data[2, 0] +
      MatrixA[i, 3] * Data[3, 0];

    NewMatrix[i, 1] :=
      MatrixA[i, 0] * Data[0, 1] +
      MatrixA[i, 1] * Data[1, 1] +
      MatrixA[i, 2] * Data[2, 1] +
      MatrixA[i, 3] * Data[3, 1];

    NewMatrix[i, 2] :=
      MatrixA[i, 0] * Data[0, 2] +
      MatrixA[i, 1] * Data[1, 2] +
      MatrixA[i, 2] * Data[2, 2] +
      MatrixA[i, 3] * Data[3, 2];

    NewMatrix[i, 3] :=
      MatrixA[i, 0] * Data[0, 3] +
      MatrixA[i, 1] * Data[1, 3] +
      MatrixA[i, 2] * Data[2, 3] +
      MatrixA[i, 3] * Data[3, 3]
  end;

  Move(NewMatrix, Data, SizeOf(TOpenGLMatrixData))
end;

procedure TOpenGLMatrix.gluPerspective(fov, aspect, zNear, zFar: GLfloat);
var
  ymax: GLfloat;
  xmax: GLfloat;
begin
  ymax := znear * tan(fov * pi / 360);
  xmax := ymax * aspect;
  glFrustum(-xmax, xmax, -ymax, ymax, znear, zfar);
end;

procedure TOpenGLMatrix.glFrustum(left, right, bottom, top, znear, zfar: GLfloat
  );
var
  temp, temp2, temp3, temp4: GLfloat;
  matrix: TOpenGLMatrix;
begin
  temp := 2.0 * znear;
  temp2 := right - left;
  temp3 := top - bottom;
  temp4 := zfar - znear;
  matrix[0, 0] := temp / temp2;
  matrix[0, 1] := 0.0;
  matrix[0, 2] := 0.0;
  matrix[0, 3] := 0.0;
  matrix[1, 0] := 0.0;
  matrix[1, 1] := temp / temp3;
  matrix[1, 2] := 0.0;
  matrix[1, 3] := 0.0;
  matrix[2, 0] := (right + left) / temp2;
  matrix[2, 1] := (top + bottom) / temp3;
  matrix[2, 2] := (-zfar - znear) / temp4;
  matrix[2, 3] := -1.0;
  matrix[3, 0] := 0.0;
  matrix[3, 1] := 0.0;
  matrix[3, 2] := (-temp * zfar) / temp4;
  matrix[3, 3] := 0.0;
  glMultMatrix(matrix);
end;

procedure TOpenGLMatrix.glRotate(a, x, y, z: GLfloat);
var
  m2: TOpenGLMatrix;
begin
  m2.glLoadIdentity;
  m2[3, 0] := x;
  m2[3, 1] := y;
  m2[3, 2] := z;
  m2[3, 3] := 1;

  glMultMatrix(m2);
end;

procedure TOpenGLMatrix.glScalef(x, y, z: GLfloat);
var
  m2: TOpenGLMatrix;
begin
  m2.glLoadIdentity;
  m2[0, 0] := x;
  m2[1, 1] := y;
  m2[2, 2] := z;
  m2[3, 3] := 1;

  glMultMatrix(m2);
end;

procedure TOpenGLMatrix.glTranslatef(x, y, z: GLfloat);
var
  m2: TOpenGLMatrix;
begin
  m2.glLoadIdentity;

  m2[3, 0] := x;
  m2[3, 1] := y;
  m2[3, 2] := z;
  m2[3, 3] := 1;

  glMultMatrix(m2);
end;

procedure TOpenGLMatrix.gluLookAt(eyeX: GLfloat; eyeY: GLfloat; eyeZ: GLfloat;
  centerX: GLfloat; centerY: GLfloat; centerZ: GLfloat; upX: GLfloat;
  upY: GLfloat; upZ: GLfloat);
var
  forward, side, up: TOpenGLVector;
  m: TOpenGLMatrix;
begin
  forward[0] := centerx - eyex;
  forward[1] := centery - eyey;
  forward[2] := centerz - eyez;

  up[0] := upx;
  up[1] := upy;
  up[2] := upz;

  forward.normalize;

  (* Side = forward x up *)
  forward.cross(up, side);
  side.normalize;

  (* Recompute up as: up = side x forward *)
  side.cross(forward, up);

  m.glLoadIdentity;
  m[0, 0] := side[0];
  m[1, 0] := side[1];
  m[2, 0] := side[2];

  m[0, 1] := up[0];
  m[1, 1] := up[1];
  m[2, 1] := up[2];

  m[0, 2] := -forward[0];
  m[1, 2] := -forward[1];
  m[2, 2] := -forward[2];

  glMultMatrix(m);
  glTranslatef(-eyex, -eyey, -eyez);
end;

{ GL_Textures }

class function GL_Textures.Texture(num: Integer): GL_TextureEnum;
begin
  Result := GL_TextureEnum(Ord(GL_TEXTURE0) + num);
end;

{ GL_Matrix }

function GL_Matrix.GetItem(x: Integer; y: Integer): GLfloat;
begin
  Result := m[y * 4 + x]
end;

procedure GL_Matrix.SetItem(x: Integer; y: Integer; AValue: GLfloat);
begin
  m[y * 4 + x] := AValue
end;
 
{$ENDREGION}
