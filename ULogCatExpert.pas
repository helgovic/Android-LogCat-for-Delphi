unit ULogCatExpert;

interface

uses
  Windows, SysUtils, Classes, vcl.Menus, vcl.ActnList, ToolsAPI, vcl.ComCtrls, vcl.ExtCtrls, vcl.Graphics, vcl.Controls,
  JclSysUtils, System.IOUtils, vcl.Dialogs, Threading, vcl.Forms, System.IniFiles, Winapi.Messages, vcl.VirtualImageList, Vcl.ImageCollection;

type

  TGraphicHack = class(TGraphic);

  TBADIToolsAPIFunctions = record
     Class Procedure RegisterFormClassForTheming(Const AFormClass : TCustomFormClass;
        Const Component : TComponent = Nil); static;
  end;

TLogCatExpert = class(TObject)
  private
    FDebugMenu,
    FMenuLogcat: TMenuItem;
    FActionLogcat: TAction;
    procedure LogcatExecute(Sender: TObject);
    procedure ExecOut(const Text: string);
    procedure ErrorOut(const Text: string);
    procedure ExecOut2(const Text: string);
    procedure ErrorOut2(const Text: string);
  protected
  public
    CheckForPid: TTimer;
    constructor Create; virtual;
    destructor Destroy; override;
    class function Instance: TLogCatExpert;
  public
    procedure StartLogCat(Clear: Boolean);
    procedure CheckForPidTimer(Sender: TObject);
  end;

var
   TmpDir: string;
   SavePid: string;

function LogCatExpert: TLogCatExpert;
function GetPackageName: String;
function GetCurrentProjectFileName: string;
function NtQueryInformationProcess(ProcessHandle: THandle;
   ProcessInformationClass: DWORD; ProcessInformation: Pointer;
   ProcessInformationLength: ULONG; ReturnLength: PULONG): LongInt; stdcall;
   external 'ntdll.dll';

implementation

uses
   JCLStrings, UFLogCat, TlHelp32, PlatformAPI;

var
   FLogCatExpert: TLogCatExpert;
   PidOutput: string;
   PackName: string;

{$IFDEF VER370}
{$ELSEIF VER360}
{$ELSEIF VER350}
{$ELSE}
function AddIconToImageList(AIcon: TIcon; ImageList: TCustomImageList;
  Stretch: Boolean): Integer;
const
  MaskColor = clBtnFace;
var
  SrcBmp, DstBmp: TBitmap;
  PSrc1, PSrc2, PDst: PRGBArray;
  X, Y: Integer;
begin
  Assert(Assigned(AIcon));
  Assert(Assigned(ImageList));

{$IFDEF DEBUG}
  if not AIcon.Empty then
    CnDebugger.LogFmt('AddIcon %dx%d To ImageList %dx%d', [AIcon.Width, AIcon.Height,
      ImageList.Width, ImageList.Height]);
{$ENDIF}

  if (ImageList.Width = 16) and (ImageList.Height = 16) and not AIcon.Empty and
    (AIcon.Width = 32) and (AIcon.Height = 32) then
  begin
    if Stretch then // ImageList 尺寸比图标大，指定拉伸的情况下，使用平滑处理
    begin
      SrcBmp := nil;
      DstBmp := nil;
      try
        SrcBmp := CreateEmptyBmp24(32, 32, MaskColor);
        DstBmp := CreateEmptyBmp24(16, 16, MaskColor);
        SrcBmp.Canvas.Draw(0, 0, AIcon);
        for Y := 0 to DstBmp.Height - 1 do
        begin
          PSrc1 := SrcBmp.ScanLine[Y * 2];
          PSrc2 := SrcBmp.ScanLine[Y * 2 + 1];
          PDst := DstBmp.ScanLine[Y];
          for X := 0 to DstBmp.Width - 1 do
          begin
            PDst^[X].b := (PSrc1^[X * 2].b + PSrc1^[X * 2 + 1].b + PSrc2^[X * 2].b
              + PSrc2^[X * 2 + 1].b) shr 2;
            PDst^[X].g := (PSrc1^[X * 2].g + PSrc1^[X * 2 + 1].g + PSrc2^[X * 2].g
              + PSrc2^[X * 2 + 1].g) shr 2;
            PDst^[X].r := (PSrc1^[X * 2].r + PSrc1^[X * 2 + 1].r + PSrc2^[X * 2].r
              + PSrc2^[X * 2 + 1].r) shr 2;
          end;
        end;
        Result := ImageList.AddMasked(DstBmp, MaskColor);
      finally
        if Assigned(SrcBmp) then FreeAndNil(SrcBmp);
        if Assigned(DstBmp) then FreeAndNil(DstBmp);
      end;
    end
    else
    begin
      // 指定不拉伸的情况下，把 32*32 图标的左上角 16*16 部分绘制来加入
      DstBmp := nil;
      try
        DstBmp := CreateEmptyBmp24(16, 16, MaskColor);
        DstBmp.Canvas.Draw(0, 0, AIcon);
        Result := ImageList.AddMasked(DstBmp, MaskColor);
      finally
        DstBmp.Free;
      end;
    end;
  end
  else if not AIcon.Empty then
    Result := ImageList.AddIcon(AIcon)
  else
    Result := -1;
end;

{$ENDIF}

function AddGraphicToVirtualImageList(Graphic: TGraphic; DstVirtual: TVirtualImageList;
  const ANamePrefix: string; Disabled: Boolean): Integer;
var
  C: Integer;
  R: TRect;
  Bmp: TBitmap;
  Mem: TMemoryStream;
  Collection: TImageCollection;
begin
  Result := -1;
  if (Graphic = nil) or (DstVirtual = nil) then
    Exit;

  if DstVirtual.ImageCollection is TImageCollection then
    Collection := DstVirtual.ImageCollection as TImageCollection
  else
    Exit;

  C := Collection.Count;
  Mem := TMemoryStream.Create;
  try
    if Graphic is TIcon then // 是 Icon 则直接存避免丢失透明度
    begin
      Mem.Clear;
      (Graphic as TIcon).SaveToStream(Mem);
    end
    else if Graphic is TBitmap then
    begin
      Mem.Clear;
      (Graphic as TBitmap).SaveToStream(Mem);
    end
    else
    begin
      Bmp := TBitmap.Create;
      try
        Bmp.PixelFormat := pf32bit;
        Bmp.AlphaFormat := afIgnored;
        Bmp.Width := Graphic.Width;
        Bmp.Height := Graphic.Height;
        R := Rect(0, 0, Bmp.Width, Bmp.Height);
        TGraphicHack(Graphic).Draw(Bmp.Canvas, R);

        Mem.Clear;
        Bmp.SaveToStream(Mem);
      finally
        Bmp.Free;
      end;
    end;
    Collection.Add(ANamePrefix + IntToStr(C), Mem);
  finally
    Mem.Free;
  end;

  DstVirtual.Add('', C, C, Disabled);
  Result := DstVirtual.Count - 1;
end;

function GetProcessCmdLine(PID: Cardinal): string;

const
   ProcessBasicInformation = 0;

type
   TPROCESSENTRY32 = record
      dwSize: DWORD;
      cntUsage: DWORD;
      th32ProcessID: DWORD; // this process
      th32DefaultHeapID: DWORD;
      th32ModuleID: DWORD; // associated exe
      cntThreads: DWORD;
      th32ParentProcessID: DWORD; // this process"s parent process
      pcPriClassBase: Longint; // Base priority of process"s threads
      dwFlags: DWORD;
      szExeFile: array [0 .. MAX_PATH - 1] of Char; // Path
   end;

   USHORT = Word;

   UNICODE_STRING = Record
      Length: USHORT;
      MaximumLength: USHORT;
      Buffer: PWideString;
   end;

   RTL_USER_PROCESS_PARAMETERS = Record
      Reserved1: array [0 .. 15] of Byte;
      Reserved2: array [0 .. 9] of Pointer;
      ImagePathName: UNICODE_STRING;
      CommandLine: UNICODE_STRING;
   end;

   PRTL_USER_PROCESS_PARAMETERS = ^RTL_USER_PROCESS_PARAMETERS;

   PEB = record
      Reserved1: array [0 .. 1] of Byte;
      BeingDebugged: ByteBool;
      Reserved2: Byte;
      Reserved3: array [0 .. 1] of Pointer;
      Ldr: Pointer;
      ProcessParameters: PRTL_USER_PROCESS_PARAMETERS;
      Reserved4: array [0 .. 103] of Byte;
      Reserved5: array [0 .. 51] of Pointer;
   end;

   PPEB = ^PEB;

   PROCESS_BASIC_INFORMATION = record
      ExitStatus: DWORD;
      PebBaseAddress: PPEB;
      AffinityMask: DWORD;
      BasePriority: DWORD;
      uUniqueProcessId: ULong;
      uInheritedFromUniqueProcessId: ULong;
   end;

   TProcessBasicInformation = PROCESS_BASIC_INFORMATION;

var
   ProcHandle: THandle;
   pbi: TProcessBasicInformation;
   ret: SIZE_T;
   ReturnLength: SIZE_T;
   WideStr: WideString;
   aPEB: PEB;
   ProcPar: RTL_USER_PROCESS_PARAMETERS;

begin

   Result := '';

   ProcHandle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ,
      False, PID);

   if ProcHandle = 0
   then
      exit;

   try

      ret := NtQueryInformationProcess(ProcHandle, ProcessBasicInformation, @pbi, SizeOf(pbi), @ReturnLength);

      if ret = 0
      then
         repeat

            if (not ReadProcessMemory(ProcHandle, pbi.PebBaseAddress, @aPEB, SizeOf(aPEB), ReturnLength)) or
               (ReturnLength <> SizeOf(aPEB))
            then
               break;

            if (not ReadProcessMemory(ProcHandle, aPEB.ProcessParameters, @ProcPar, SizeOf(ProcPar), ReturnLength)) or
               (ReturnLength <> SizeOf(ProcPar))
            then
               break;

            SetLength(WideStr, ProcPar.CommandLine.Length div 2);

            if (not ReadProcessMemory(ProcHandle, ProcPar.CommandLine.Buffer, PWideChar(WideStr), ProcPar.CommandLine.Length, ReturnLength)) or
               (ReturnLength <> ProcPar.CommandLine.Length)
            then
               break;

            Result := WideStr;

         until True;

   finally
      CloseHandle(ProcHandle);
   end;

end;

function KillTask(ExeFileName: string): Integer;

const
   PROCESS_TERMINATE = $0001;

var
   ContinueLoop: Boolean;
   FSnapshotHandle: THandle;
   FProcessEntry32: TProcessEntry32;

begin

   Result := 0;
   FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
   FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
   ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);

   while ContinueLoop do
   begin

      if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile)) = UpperCase(ExeFileName)) or
          (UpperCase(FProcessEntry32.szExeFile) = UpperCase(ExeFileName)))
      then
         if (StrFind('adb.exe"  shell logcat V', GetProcessCmdLine(FProcessEntry32.th32ProcessID)) > 0) or
            (StrFind('adb.exe"  shell logcat --pid ', GetProcessCmdLine(FProcessEntry32.th32ProcessID)) > 0)
         then
            begin

               Result := Integer(TerminateProcess(OpenProcess(PROCESS_TERMINATE,
                                                           BOOL(0),
                                                           FProcessEntry32.th32ProcessID),
                                                           0));

               Result := Integer(TerminateProcess(OpenProcess(PROCESS_TERMINATE,
                                                           BOOL(0),
                                                           FProcessEntry32.th32ParentProcessID),
                                                           0));
            end;

      ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);

   end;

  CloseHandle(FSnapshotHandle);

end;

function FindMenuItem(MenuCaptions: String): TMenuItem;

var
   Captions: TStringList;
   NTAServices: INTAServices;
   y, i: integer;
   MenuItems: TMenuItem;
   Caption: String;
   Found: Boolean;

begin

   Result := nil;

   if Supports(BorlandIDEServices, INTAServices, NTAServices)
   then
      begin

         Captions := TStringList.Create;
         Captions.Delimiter := ';';
         Captions.StrictDelimiter := True;
         Captions.DelimitedText := MenuCaptions;

         MenuItems := NTAServices.MainMenu.Items;

         for y := 0 to Captions.Count - 1 do
            begin

               Found := False;

               for i := 0 to MenuItems.Count - 1 do
                  begin

                     Caption := StringReplace(MenuItems.Items[i].Caption, '&', '', []);

                     if Uppercase(Caption) = Uppercase(Captions[y])
                     then
                        begin
                           MenuItems := MenuItems.Items[i];
                           Found := True;
                           Break;
                        end;

                  end;

               if not Found
               then
                  begin
                     Captions.Free;
                     Exit;
                  end;

            end;

         Result := MenuItems;
         Captions.Free;

      end;

end;

function GetProjectGroup: IOTAProjectGroup;

var
   IModuleServices: IOTAModuleServices;
   IModule: IOTAModule;
   i: Integer;

begin

   IModuleServices := BorlandIDEServices as IOTAModuleServices;

   Result := nil;

   for i := 0 to IModuleServices.ModuleCount - 1 do
      begin

         IModule := IModuleServices.Modules[i];

         if IModule.QueryInterface(IOTAProjectGroup, Result) = S_OK
         then
            Break;

      end;

end;

function GetCurrentProject: IOTAProject;

var
   Project: IOTAProject;
   ProjectGroup: IOTAProjectGroup;

begin

   Result := nil;

   ProjectGroup := GetProjectGroup;

   if Assigned(ProjectGroup)
   then
      begin

         Project := ProjectGroup.ActiveProject;

         if Assigned(Project)
         then
            Result := Project;

      end;

end;

function GetCurrentProjectFileName: string;

var

  IProject: IOTAProject;

begin

  Result := '';

  IProject := GetCurrentProject;

  if Assigned(IProject) then
  begin
     Result := IProject.FileName;
  end;

end;

function GetPackageName: String;

var
   ProjectOptionsConfigurations: IOTAProjectOptionsConfigurations;
   Found: Boolean;
   x, y: Integer;
   ProjPackage, TmpStr: String;

begin

   Result := '';

   If Supports(GetActiveProject.ProjectOptions, IOTAProjectOptionsConfigurations, ProjectOptionsConfigurations)
   Then
      begin

         Found := False;

         for x := 0 to ProjectOptionsConfigurations.ConfigurationCount - 1 do
            begin

              if ProjectOptionsConfigurations.Configurations[x].Name = GetCurrentProject.CurrentConfiguration
              then
                 begin

                    if ProjectOptionsConfigurations.Configurations[x].PlatformConfiguration[GetCurrentProject.CurrentPlatform] <> nil
                    then
                       begin

                          for y := 0 to ProjectOptionsConfigurations.Configurations[x].PlatformConfiguration[GetCurrentProject.CurrentPlatform].PropertyCount - 1 do
                             begin

                                if ProjectOptionsConfigurations.Configurations[x].PlatformConfiguration[GetCurrentProject.CurrentPlatform].Properties[y] = 'VerInfo_Keys'
                                then
                                   begin

                                      Found := True;

                                      ProjPackage := StrBefore(';', StrAfter('package=', ProjectOptionsConfigurations.Configurations[x].PlatformConfiguration[GetCurrentProject.CurrentPlatform].GetValue(ProjectOptionsConfigurations.Configurations[x].PlatformConfiguration[GetCurrentProject.CurrentPlatform].Properties[y], True)));

                                      if Pos('$(MSBuildProjectName)', ProjPackage) > 0
                                      then
                                         ProjPackage := StringReplace(ProjPackage, '$(MSBuildProjectName)', StrBefore('.dproj', ExtractFileName(GetCurrentProjectFileName)), []);

                                      if Pos('$(ModuleName)', ProjPackage) > 0
                                      then
                                         ProjPackage := StringReplace(ProjPackage, '$(ModuleName)', StrBefore('.dproj', ExtractFileName(GetCurrentProjectFileName)), []);;

                                      Result := ProjPackage;

                                      Break;

                                   end;

                             end;

                       end;

                    Break;

                 end;
            end;

         if not Found
         then
            for x := 0 to ProjectOptionsConfigurations.ConfigurationCount - 1 do
            begin

              if ProjectOptionsConfigurations.Configurations[x].Name = 'Base'
              then
                 begin

                    if ProjectOptionsConfigurations.Configurations[x].PlatformConfiguration[GetCurrentProject.CurrentPlatform] <> nil
                    then
                       begin

                          for y := 0 to ProjectOptionsConfigurations.Configurations[x].PlatformConfiguration[GetCurrentProject.CurrentPlatform].PropertyCount - 1 do
                             begin

                                if ProjectOptionsConfigurations.Configurations[x].PlatformConfiguration[GetCurrentProject.CurrentPlatform].Properties[y] = 'VerInfo_Keys'
                                then
                                   begin

                                      TmpStr := StrBefore(';', StrAfter('package=', ProjectOptionsConfigurations.Configurations[x].PlatformConfiguration[GetCurrentProject.CurrentPlatform].GetValue(ProjectOptionsConfigurations.Configurations[x].PlatformConfiguration[GetCurrentProject.CurrentPlatform].Properties[y], True)));

                                      if Pos('$(MSBuildProjectName)', TmpStr) > 0
                                      then
                                         TmpStr := StringReplace(TmpStr, '$(MSBuildProjectName)', StrBefore('.dproj', ExtractFileName(GetCurrentProjectFileName)), []);

                                      if Pos('$(ModuleName)', TmpStr) > 0
                                      then
                                         TmpStr := StringReplace(TmpStr, '$(ModuleName)', StrBefore('.dproj', ExtractFileName(GetCurrentProjectFileName)), []);

                                      Result := TmpStr;

                                      Break;

                                   end;

                             end;

                       end;

                    Break;

                 end;

            end;

      end;

end;

procedure TLogCatExpert.LogcatExecute(Sender: TObject);
begin

   TFLogCat.ShowDockableLogCat;

end;

procedure TLogCatExpert.ExecOut2(const Text: string);
begin

   if (Pos('adb.exe" shell logcat -c', Text) > 0) or
      (Pos('adb.exe" shell pidof ', Text) > 0)
   then
      Exit;

   PidOutput := PidOutput + Text;

end;

procedure TLogCatExpert.ExecOut(const Text: string);

var
   MessRec: TMessRec;

begin

   if Text <> ''
   then
      begin

         if (Pos('adb.exe" shell logcat -c', Text) > 0) or
            (Pos('adb.exe" shell logcat V', Text) > 0) or
            (Pos('adb.exe" shell logcat --pid ', Text) > 0)
         then
            Exit;

         if Pos(' V ', Text) > 0
         then
            MessRec.LogLvl := 0
         else
            if Pos(' D ', Text) > 0
            then
               MessRec.LogLvl := 1
            else
               if Pos(' W ', Text) > 0
               then
                  MessRec.LogLvl := 2
               else
                  if Pos(' I ', Text) > 0
                  then
                     MessRec.LogLvl := 3
                  else
                     if Pos(' E ', Text) > 0
                     then
                        MessRec.LogLvl := 4
                     else
                        if Pos(' F ', Text) > 0
                        then
                           MessRec.LogLvl := 5
                        else
                           MessRec.LogLvl := 6;

         MessRec.LCMess := Text;

         LCMessages.Add(MessRec);

      end;

end;

procedure TLogCatExpert.ErrorOut2(const Text: string);
begin

   if Pos('* daemon', Text) > 0
   then
      Exit;

   NoDevFound := True;

   if Pos('no devices/emulators found', Text) > 0
   then
      ShowMessage(Text)
   else
      ShowMessage('Error rertreiving pid' + Text);

end;

procedure TLogCatExpert.ErrorOut(const Text: string);
begin

   if Pos('* daemon', Text) > 0
   then
      Exit;

   NoDevFound := True;

   if Pos('no devices/emulators found', Text) > 0
   then
      ShowMessage(Text)
   else
      ShowMessage('Error running logcat ' + Text);

end;

function LogCatExpert: TLogCatExpert;
begin
  Result := TLogCatExpert.Instance;
end;

procedure TLogCatExpert.StartLogCat(Clear: Boolean);

var
   FileLines: TStringList;
   PlatformSDKServices: IOTAPlatformSDKServices;
   AndroidSDK: IOTAPlatformSDKAndroid;
   adbPath: string;

begin

   BreakMsgs := True;

   KillTask('adb.exe');

   FileLines := TStringList.Create;
   FileLines.Clear;

   PlatformSDKServices := (BorlandIDEServices as IOTAPlatformSDKServices);
   AndroidSDK := PlatformSDKServices.GetDefaultForPlatform('Android') as IOTAPlatformSDKAndroid;
   adbPath := '"' + StrBefore('\build-tools\', AndroidSDK.SDKAaptPath) + '\platform-tools\adb.exe"';

   if Clear
   then
      FileLines.Add('adb shell logcat -c');

   if not FormInstance.CBThisPack.Checked
   then
      FileLines.Add('adb shell logcat V')
   else
      if SavePid = ''
      then
         begin

            LCMessages.Clear;

            FormInstance.VSTLogCat.Clear;
            FormInstance.VSTLogCat.Header.Columns[0].Width := FormInstance.VSTLogCat.Width - 30;

            if not FormInstance.TBClear.Enabled
            then
               FormInstance.TBClear.Enabled := True;

            Exit;

         end
      else
         FileLines.Add('adb shell logcat --pid ' + SavePid);

   FileLines.SaveToFile(TmpDir + '\StartLogCat.bat');
   FileLines.Free;

   LCMessages.Clear;

   StartMessageLoop;

   if not FormInstance.TBClear.Enabled
   then
      FormInstance.TBClear.Enabled := True;

   TThread.CreateAnonymousThread(procedure()
   begin

      Execute(TmpDir + '\StartLogCat.bat', ExecOut, ErrorOut);

   end).Start;

end;

procedure TLogCatExpert.CheckForPidTimer(Sender: TObject);

var
   FileLines: TStringList;
   TmpStr: string;
   PlatformSDKServices: IOTAPlatformSDKServices;
   AndroidSDK: IOTAPlatformSDKAndroid;
   adbPath: string;

begin

   if NoDevFound
   then
      Exit;

   if GetCurrentProjectFileName = ''
   then
      Exit;

   CheckForPid.Enabled := False;

   PackName := GetPackageName;

   if PackName = ''
   then
      begin

         if FormInstance.CBThisPack.Checked
         then
            CheckForPid.Enabled := True;

         Exit;

      end;

   PlatformSDKServices := (BorlandIDEServices as IOTAPlatformSDKServices);
   AndroidSDK := PlatformSDKServices.GetDefaultForPlatform('Android') as IOTAPlatformSDKAndroid;
   adbPath := '"' + StrBefore('\build-tools\', AndroidSDK.SDKAaptPath) + '\platform-tools\adb.exe"';

   PidOutput := '';

   FileLines := TStringList.Create;
   FileLines.Clear;

   FileLines.Add('adb shell pidof ' + PackName);
   FileLines.SaveToFile(TmpDir + '\GetPid.bat');
   FileLines.Free;

   if Execute(TmpDir + '\GetPid.bat', ExecOut2, ErrorOut2) = 0
   then
      begin

         if FormInstance.CBThisPack.Checked
         then
            begin

               FileLines := TStringList.Create;
               FileLines.Clear;

               StrToStrings(PidOutput, ' ', FileLines, True);
               TmpStr := Trim(FileLines[FileLines.Count - 1]);
               FileLines.Free;

               if StrIsDigit(TmpStr)
               then
                  if SavePid <> TmpStr
                  then
                     begin

                        SavePid := TmpStr;
                        FormInstance.TBClear.Enabled := False;
                        StartLogCat(False);

                     end
                  else
               else
                  SavePid := '';

               CheckForPid.Enabled := True;

            end;

      end
   else
      if FormInstance.CBThisPack.Checked
      then
         CheckForPid.Enabled := True;

end;

class function TLogCatExpert.Instance: TLogCatExpert;
begin

   if FLogCatExpert = nil
   then
      FLogCatExpert := TLogCatExpert.Create;

  Result := FLogCatExpert;

end;

constructor TLogCatExpert.Create;

var
   NTAServices : INTAServices;
   Bmp: TBitmap;
   ImageIndex: integer;

begin

   inherited Create;

   TmpDir := 'C:\Users\Public\Documents\Embarcadero\Studio\Logcat';

   if not DirectoryExists(TmpDir)
   then
      if not CreateDir(TmpDir)
      then
         begin
            ShowMessage('Error creating ' + TmpDir);
            Exit;
         end;

   if Supports(BorlandIDEServices, INTAServices, NTAServices)
   then
      begin

         FDebugMenu := FindMenuItem('View;Debug Windows;Breakpoints');

         Bmp := TBitmap.Create;

         FActionLogcat := TAction.Create(nil);
         FActionLogcat.Category := 'View';
         FActionLogcat.Caption := 'Logcat';
         FActionLogcat.Hint := 'Logcat for Delphi';
         FActionLogcat.Name := 'LogcatAction';
         FActionLogcat.Visible := True;
         FActionLogcat.OnExecute := LogcatExecute;
         FActionLogcat.Enabled := True;

         FMenuLogcat := TMenuItem.Create(nil);
         FMenuLogcat.Name := 'Logcat';
         FMenuLogcat.Caption := 'Logcat for Delphi';
         FMenuLogcat.AutoHotkeys := maAutomatic;
         FMenuLogcat.Action := FActionLogcat;

         NTAServices.AddActionMenu(FDebugMenu.Name, FActionLogcat, FMenuLogcat, True);

         Bmp.LoadFromResourceName(HInstance, 'Logcaticon');

         {$IFDEF VER370}
             ImageIndex := AddGraphicToVirtualImageList(bmp, NTAServices.ImageList as TVirtualImageList, '', False);
         {$ELSEIF VER360}
             ImageIndex := AddGraphicToVirtualImageList(bmp, NTAServices.ImageList as TVirtualImageList, '', False);
         {$ELSEIF VER350}
             ImageIndex := AddGraphicToVirtualImageList(bmp, NTAServices.ImageList as TVirtualImageList, '', False);
         {$ELSE}
             ImageIndex := AddIconToImageList(bmp, NTAServices.ImageList as TVirtualImageList, False);
         {$ENDIF}

         FActionLogcat.ImageIndex := ImageIndex;
         FMenuLogcat.ImageIndex := ImageIndex;

         Bmp.Free;

      end;

   TFLogCat.CreateDockableLogCat;
   TBADIToolsAPIFunctions.RegisterFormClassForTheming(TFLogCat, FormInstance);

   CheckForPid := TTimer.Create(nil);
   CheckForPid.Enabled := False;
   CheckForPid.Interval := 2000;
   CheckForPid.OnTimer := CheckForPidTimer;

end;

destructor TLogCatExpert.Destroy;
begin

   BreakMsgs := True;

   KillTask('adb.exe');

   TFLogCat.RemoveDockableLogCat;

   FMenuLogcat.Free;
   FActionLogcat.Free;

   inherited Destroy;

end;

class procedure TBADIToolsAPIFunctions.RegisterFormClassForTheming(
  const AFormClass: TCustomFormClass; const Component: TComponent);
begin

   {$IFDEF Ver320}
   Var
     ITS : IOTAIDEThemingServices250;
   {$ENDIF Ver320}
   {$IFDEF Ver330}
   Var
     ITS : IOTAIDEThemingServices250;
   {$ENDIF Ver330}
   {$IFDEF Ver340}
   Var
     ITS : IOTAIDEThemingServices;
  {$ENDIF Ver340}
   {$IFDEF Ver350}
   Var
     ITS : IOTAIDEThemingServices;
  {$ENDIF Ver350}

   Begin

     {$IFDEF Ver350}
     If Supports(BorlandIDEServices, IOTAIDEThemingServices, ITS) Then
       If ITS.IDEThemingEnabled Then
         Begin
           ITS.RegisterFormClass(AFormClass);
           If Assigned(Component) Then
             ITS.ApplyTheme(Component);
         End;
     {$ENDIF Ver350}

     {$IFDEF Ver340}
     If Supports(BorlandIDEServices, IOTAIDEThemingServices, ITS) Then
       If ITS.IDEThemingEnabled Then
         Begin
           ITS.RegisterFormClass(AFormClass);
           If Assigned(Component) Then
             ITS.ApplyTheme(Component);
         End;
     {$ENDIF Ver340}

     {$IFDEF Ver330}
     If Supports(BorlandIDEServices, IOTAIDEThemingServices250, ITS) Then
       If ITS.IDEThemingEnabled Then
         Begin
           ITS.RegisterFormClass(AFormClass);
           If Assigned(Component) Then
             ITS.ApplyTheme(Component);
         End;
     {$ENDIF Ver330}
     {$IFDEF Ver320}
     If Supports(BorlandIDEServices, IOTAIDEThemingServices250, ITS) Then
       If ITS.IDEThemingEnabled Then
         Begin
           ITS.RegisterFormClass(AFormClass);
           If Assigned(Component) Then
             ITS.ApplyTheme(Component);
         End;
     {$ENDIF Ver320}

   End;

end;

initialization
  FLogCatExpert := TLogCatExpert.Instance;
finalization
   TFLogCat.RemoveDockableLogCat;
   FreeAndNil(FLogCatExpert);

end.



