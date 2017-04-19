unit SDL2.DesignStrings;

{$mode objfpc}{$H+}

interface

resourcestring
  SDLMenuCategory = 'SDL2';
  SDLProject = 'SDL2 Application';
  SDLProjectDescription = 'Create SDL2 project';
  SDLWindow = 'SDL Window';
  SDLWindowDescription = 'Add new SDL window to project';
  SDL2Application = 'SDL2 Application';
  rsAndroidDir = 'Android Dir: %s';
  ErrAndroidSDKNotSet = 'Android SDK not set';
  rsErrUnitOutputDirectoryNotSet = 'Could not find unit output directory';
  rsSetAndroidEnvOptionGeneral= 'Set android SDK at Tools->Options->Android->General';
  rsSetAndroidProjectOptionGeneral = 'Set android SDK at Tools->Options->Android (Project)->General';
  rsSetProjectOptionVersionInfo = 'Set build number at Project->Project Options->Project Options->Version Info';
  rsCreatingDirectoryStructure = 'Creating project directory Structure';
  ErrCouldNotCreateDirectory = 'Could not create directory: %s';
  rsDirectoryCreated = 'Directory created: %s';
  rsCopyLibraries = 'Copy libraries';
  rsErrFailedToDeleteFile = 'Failed to delete old file "%s"';
  rsErrFailedToCopyFile = 'Failed to copy from "%s" to "%s"';
  rsFileCopied = 'File copied from "%s" to "%s"';
  rsCopyingResources = 'Copying resources';
  rsUsingLazarusIcon = 'Project icon did not found. Using lazarus icon';
  rsErrCouldNotFounLazarusIcon = 'Could not found "%s" icon file';
  rsCopyingJavaSources = 'Copying Java source';
  rsErrAndroidSDKNotFound = 'Android SDK not found at %s';
  rsBuildingAndroid = 'Building android';
  rsErrAndroidTargetNotFound = 'Android target not found';
  rsErrFileNotFound = 'File not found: %s';
  rsErrTargetFileNoFound = 'Target file not found!';
  rsErrBuildNumberIsMandatory = 'Build number is mandatory for Android projects';
  rsErrNoVersionInfo = 'No version information entered';
  rsAndroidPacka = 'Android package name not found';
  rsErrOpenGLESVersionNotSet = 'OpenGL ES version is not set';
  rsErrMainActivityNotSet = 'Main activity not set';
  rsErrScreenSettingsNotSet = 'Screen settings not set';
  rsErrCouldNotCopyFile = 'Could not copy file "%s" to "%s". (%d)';
  rsErrPathToAntNotSet = 'Path to Ant not set';

implementation

end.

