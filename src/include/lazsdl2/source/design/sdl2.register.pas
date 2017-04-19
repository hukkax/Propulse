unit SDL2.Register;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FormEditingIntf, ProjectIntf, LazIDEIntf, NewItemIntf,
  MacroIntf, MacroDefIntf;

type

  { TSDLWindowDescriptor }

  TSDLWindowDescriptor = class(TFileDescPascalUnitWithResource)
  public
    constructor Create; override;
    function GetLocalizedName : String; override;
    function GetLocalizedDescription : String; override;
    function GetInterfaceUsesSection : String; override;
    function GetImplementationSource(const Filename, SourceName, ResourceName: string): string; override;
  end;

procedure Register;

implementation

uses
  SDL.Extended.Window, SDL2.PropEdits, PropEdits, SDL2.Project, SDL2.DesignStrings, IDEOptionsIntf,
  LResources, SDL.Extended.LibraryProvider, SDL.Extended.Thread, SDL.Extended.AtomicLock,
  SDL.Extended.Mutex, SDL.Extended.Semaphore, SDL.Extended.ConditionVariable,
  SDL.Extended.Renderer, SDL.Extended.Camera, SDL.Extended.WorkerControl, SDL.Extended.OpenGLExtensions,
  SDL.Android.Options.Enviorment, SDL.Android.Options.Enviorment.Fm, SDL.Android.JavaSource,
  SDL.Android.Options.Project, SDL.Android.Options.Project.Fm, Dialogs, SDL.Android.Options.Libraries.Fm,
  SDL.Android.Permissions.Fm;

procedure Register;
var
  EnvOptions: TAndroidEnviormentOptions;
  ProjOptions: TAndroidProjectOptions;
begin
  RegisterNewItemCategory(TNewIDEItemCategory.Create(SDLMenuCategory));
  RegisterProjectDescriptor(TSDL2ProjectDescriptor.Create, SDLMenuCategory);
  RegisterProjectFileDescriptor(TSDLWindowDescriptor.Create, SDLMenuCategory);
  RegisterProjectFileDescriptor(TJavaSourceFile.Create, SDLMenuCategory);
  FormEditingHook.RegisterDesignerBaseClass(TSDLWindow);

  RegisterPropertyEditor(TypeInfo(TSDLCustomLibraryProvider), nil, '', TSDLLibraryProviderEditor);
  RegisterPropertyEditor(TypeInfo(TSDLWindow.TSDLWindowSize), nil, '', TSDLClassPropEdit);
  RegisterPropertyEditor(TypeInfo(TSDLWindow.TSDLWindowPosition), nil, '', TSDLClassPropEdit);
  RegisterPropertyEditor(TypeInfo(TSDLCamera.TSDLCameraCenter), nil, '', TSDLClassPropEdit);
  RegisterPropertyEditor(TypeInfo(TSDLCamera.TSDLCameraEye), nil, '', TSDLClassPropEdit);

  {$I ../extended/sdl.extended.threadprovider_icon.lrs}
  {$I ../extended/sdl.extended.thread_icon.lrs}
  {$I ../extended/sdl.extended.atomiclock_icon.lrs}
  {$I ../extended/sdl.extended.mutex_icon.lrs}
  {$I ../extended/sdl.extended.semaphore_icon.lrs}
  {$I ../extended/sdl.extended.conditionvariable_icon.lrs}
  {$I ../extended/sdl.extended.renderer_icon.lrs}
  {$I ../extended/sdl.extended.camera_icon.lrs}
  {$I ../extended/sdl.extended.workercontrol_icon.lrs}
  {$I ../extended/sdl.extended.openglextensions_icon.lrs}
  RegisterComponents('SDL',[TSDLLibraryProvider, TSDLOpenGLExtensions, TSDLRenderer, TSDLCamera]);
  RegisterComponents('SDL process',[TSDLThread, TSDLWorkerControl]);
  RegisterComponents('SDL sync',[TSDLAtomicLock, TSDLMutex, TSDLSemaphore, TSDLConditionVariable]);

  with RegisterIDEOptionsGroup(GroupEnvironment, TAndroidEnviormentOptions)^ do
  begin
    Items := TIDEOptionsEditorList.Create;
    RegisterIDEOptionsEditor(Index, TfrmAndroidOptions, GetFreeIDEOptionsIndex(Index, Index));
    //RegisterIDEOptionsEditor(Index, TfrmSources, GetFreeIDEOptionsIndex(Index, Index));
  end;

  with RegisterIDEOptionsGroup(GroupEnvironment{GroupProject}, TAndroidProjectOptions)^ do
  begin
    Items := TIDEOptionsEditorList.Create;
    RegisterIDEOptionsEditor(Index, TfrmAndroidProjectOptions, GetFreeIDEOptionsIndex(Index, Index));
    RegisterIDEOptionsEditor(Index, TfrmAndroidLibraries, GetFreeIDEOptionsIndex(Index, Index));
    RegisterIDEOptionsEditor(Index, TfrmAndroidPermissions, GetFreeIDEOptionsIndex(Index, Index))
  end;

  ProjOptions := TAndroidProjectOptions(TAndroidProjectOptions.GetInstance);
  if Assigned(ProjOptions) then
  begin
    LazarusIDE.AddHandlerOnProjectOpened(@ProjOptions.ExternalOnProjectOpened);
    LazarusIDE.AddHandlerOnProjectClose(@ProjOptions.ExternalOnProjectClosed);
    LazarusIDE.AddHandlerOnProjectBuildingFinished(@ProjOptions.ExternalAfterBuild);
    LazarusIDE.AddHandlerOnSavedAll(@ProjOptions.ExternalOnSaveAll);
  end;

  EnvOptions := TAndroidEnviormentOptions(TAndroidEnviormentOptions.GetInstance);
  if Assigned(EnvOptions) then
  begin
    LazarusIDE.AddHandlerOnProjectOpened(@EnvOptions.ExternalOnProjectOpened);
    LazarusIDE.AddHandlerOnProjectClose(@EnvOptions.ExternalOnProjectClosed)
  end;

  if not IDEMacros.IsMacro('Android') then
    IDEMacros.Add(TTransferMacro.Create('Android', '', 'Get Android enviorment variables', @EnvOptions.ExtenalAndroidMacro, []));

  if not IDEMacros.IsMacro('AndroidProject') then
    IDEMacros.Add(TTransferMacro.Create('AndroidProject', '', 'Get Android project variables', @ProjOptions.ExtenalAndroidMacro, []));

  if not IDEMacros.IsMacro('Lib') then
    IDEMacros.Add(TTransferMacro.Create('Lib', '', 'Get library name do not change case', @ProjOptions.ExtenalLibraryMacro, []));

  if not IDEMacros.IsMacro('TargetDir') then
    IDEMacros.Add(TTransferMacro.Create('TargetDir', '', 'Get target directory', @ProjOptions.ExtenalTargetDirMacro, []));
end;

{ TSDLWindowDescriptor }

constructor TSDLWindowDescriptor.Create;
begin
  inherited Create;
  Name := 'SDL Window';
  ResourceClass := TSDLWindow;
  UseCreateFormStatements:=False;
end;

function TSDLWindowDescriptor.GetLocalizedName: String;
begin
  Result := SDLWindow
end;

function TSDLWindowDescriptor.GetLocalizedDescription: String;
begin
  Result := SDLWindowDescription
end;

function TSDLWindowDescriptor.GetInterfaceUsesSection: String;
begin
  Result := inherited GetInterfaceUsesSection + ', SDL.Extended.Window, SDL.Api.Types';
end;

function TSDLWindowDescriptor.GetImplementationSource(const Filename,
  SourceName, ResourceName: string): string;
var
  Source: TStringList;
begin
  Result := inherited GetImplementationSource(Filename, SourceName, ResourceName);
  Source := TStringList.Create;
  try
    begin
    Source.Add('uses');
    Source.Add('  SDL.Extended.Application;');
    Source.Add('');

    Source.Add(Result);

    Source.Add('initialization');
    Source.Add('  Application.RegisterWindow(T' + ResourceName + ', True);');

    end;
    Result := Source.Text;
  finally
    Source.Free;
  end;
end;

end.

