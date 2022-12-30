unit UFLogCat;

interface

uses
   Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
   System.Classes, Vcl.Graphics,
   Vcl.Controls, Vcl.Forms, Vcl.Dialogs, DockForm, Vcl.StdCtrls, Vcl.ComCtrls,
   Vcl.ExtCtrls, Vcl.Imaging.pngimage, Vcl.ToolWin, System.ImageList, Vcl.ImgList,
   VirtualTrees, Vcl.Imaging.jpeg, System.Generics.Collections, Vcl.Buttons, ToolsAPI,
   Vcl.Menus, vcl.Clipbrd;

type
   TFLogCat = class(TDockableForm)
    Panel1: TPanel;
    CBFilter: TComboBox;
    CBLogLevel: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    ToolBar1: TToolBar;
    TBStopScroll: TToolButton;
    TBClear: TToolButton;
    ImageList1: TImageList;
    VSTLogCat: TVirtualStringTree;
    CBThisPack: TCheckBox;
    BApplyFilter: TSpeedButton;
    PUMLogCat: TPopupMenu;
    MISavetofile: TMenuItem;
    SDLogCat: TSaveDialog;
    MIAllToClipBrd: TMenuItem;
    MIMessToClipBrd: TMenuItem;
    procedure TBClearClick(Sender: TObject);
    procedure CBLogLevelSelect(Sender: TObject);
    procedure VSTLogCatDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; const Text: string;
      const CellRect: TRect; var DefaultDraw: Boolean);
    procedure VSTLogCatGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VSTLogCatGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure CBFilterDropDown(Sender: TObject);
    procedure CBFilterSelect(Sender: TObject);
    procedure CBThisPackClick(Sender: TObject);
    procedure BApplyFilterClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MISavetofileClick(Sender: TObject);
    procedure MIAllToClipBrdClick(Sender: TObject);
    procedure MIMessToClipBrdClick(Sender: TObject);
      private
      public
         Constructor Create(AOwner: TComponent); Override;
         Destructor Destroy; Override;
         Procedure Focus;
         Class Procedure ShowDockableLogCat;
         Class Procedure RemoveDockableLogCat;
         Class Procedure CreateDockableLogCat;
   end;

   TFLogCatClass = Class of TFLogCat;

type
   TMessRec = record
      LogLvl: Integer;
      LCMess: string;
   end;

type
  rTreeData = record
  idx: Integer;
end;

PTreeData = ^rTreeData;

var
   LCMessages: TList<TMessRec>;
   FormInstance: TFLogCat;
   BreakMsgs: Boolean = False;
   CurrMessIdx: Integer;

procedure StartMessageLoop;
procedure RestartMessageLoop;

implementation

{$R *.dfm}

Uses
   DeskUtil, JclSysUtils, ULogCatExpert, BrandingAPI, System.IniFiles, JclStrings;

var
  FilterText: string = '';

procedure RestartMessageLoop;
begin

   BreakMsgs := True;

   Sleep(200);

   StartMessageLoop;

end;

Procedure RegisterDockableForm(FormClass: TFLogCatClass;
   var FormVar; Const FormName: String);
Begin

   If @RegisterFieldAddress <> Nil
   Then
      RegisterFieldAddress(FormName, @FormVar);

   RegisterDesktopFormClass(FormClass, FormName, FormName);

End;

Procedure UnRegisterDockableForm(var FormVar; Const FormName: String);
Begin

   If @UnRegisterFieldAddress <> Nil
   Then
      UnRegisterFieldAddress(@FormVar);

End;

Procedure CreateDockableForm(var FormVar: TFLogCat; FormClass: TFLogCatClass);
Begin

   TCustomForm(FormVar) := FormClass.Create(Nil);

   RegisterDockableForm(FormClass, FormVar, TCustomForm(FormVar).Name);

End;

Procedure FreeDockableForm(var FormVar: TFLogCat);
Begin

   If Assigned(FormVar)
   Then
      Begin

         UnRegisterDockableForm(FormVar, FormVar.Name);

         FreeAndNil(FormVar);

      End;

End;

Procedure ShowDockableForm(Form: TFLogCat);
Begin

   If Not Assigned(Form)
   Then
      Exit;

   If Not Form.Floating
   Then
      Begin

         Form.ForceShow;

         FocusWindow(Form);

         Form.Focus;

      End
   Else
      Begin

         Form.Show;

         Form.Focus;

      End;

End;

procedure StartMessageLoop;
begin

   CurrMessIdx := 0;
   FormInstance.VSTLogCat.Clear;
   FormInstance.VSTLogCat.Header.Columns[0].Width := FormInstance.VSTLogCat.Width - 30;
   BreakMsgs := False;

   TThread.CreateAnonymousThread(procedure()
   begin

      while not BreakMsgs do
         if CurrMessIdx <= LCMessages.Count - 1
         then
            begin

               if LCMessages[CurrMessIdx].LCMess <> ''
               then
                  begin

                     TThread.Synchronize(TThread.CurrentThread,
                     procedure ()

                     var
                       Node: PVirtualNode;
                       NodeData: PTreeData;

                     begin

                        if LCMessages.Count > 10000
                        then
                           begin
                              LCMessages.Delete(0);
                              Dec(CurrMessIdx);
                           end
                        else
                           begin

                              if ((FormInstance.CBLogLevel.ItemIndex = 0) or
                                  ((FormInstance.CBLogLevel.ItemIndex = 1) and
                                   ((LCMessages[CurrMessIdx].LogLvl = 1) or
                                    (LCMessages[CurrMessIdx].LogLvl = 2) or
                                    (LCMessages[CurrMessIdx].LogLvl = 3) or
                                    (LCMessages[CurrMessIdx].LogLvl = 4) or
                                    (LCMessages[CurrMessIdx].LogLvl = 5) or
                                    (LCMessages[CurrMessIdx].LogLvl = 6))) or
                                  ((FormInstance.CBLogLevel.ItemIndex = 2) and
                                   ((LCMessages[CurrMessIdx].LogLvl = 2) or
                                    (LCMessages[CurrMessIdx].LogLvl = 3) or
                                    (LCMessages[CurrMessIdx].LogLvl = 4) or
                                    (LCMessages[CurrMessIdx].LogLvl = 5) or
                                    (LCMessages[CurrMessIdx].LogLvl = 6))) or
                                  ((FormInstance.CBLogLevel.ItemIndex = 3) and
                                   ((LCMessages[CurrMessIdx].LogLvl = 3) or
                                    (LCMessages[CurrMessIdx].LogLvl = 4) or
                                    (LCMessages[CurrMessIdx].LogLvl = 5) or
                                    (LCMessages[CurrMessIdx].LogLvl = 6))) or
                                  ((FormInstance.CBLogLevel.ItemIndex = 4) and
                                   ((LCMessages[CurrMessIdx].LogLvl = 4) or
                                    (LCMessages[CurrMessIdx].LogLvl = 5) or
                                    (LCMessages[CurrMessIdx].LogLvl = 6))) or
                                  ((FormInstance.CBLogLevel.ItemIndex = 5) and
                                    ((LCMessages[CurrMessIdx].LogLvl = 5) or
                                    (LCMessages[CurrMessIdx].LogLvl = 6)))) and
                                 ((FilterText = '') or
                                  (StrFind(StrLower(FilterText), StrLower(LCMessages[CurrMessIdx].LCMess)) > 0))
                              then
                                 begin
                                    Node := FormInstance.VSTLogCat.AddChild(nil);
                                    NodeData := FormInstance.VSTLogCat.GetNodeData(Node);
                                    NodeData.idx := CurrMessIdx;
                                 end;

                           end;

                        FormInstance.VSTLogCat.Refresh;

                        if not FormInstance.TBStopScroll.Down
                        then
                           FormInstance.VSTLogCat.ScrollIntoView(FormInstance.VSTLogCat.GetLast, False);

                        Inc(CurrMessIdx);

                     end);

                  end;

            end;

   end).Start;

end;

procedure TFLogCat.BApplyFilterClick(Sender: TObject);

var
   Items: TStringList;
   i: integer;
   Found: Boolean;

begin

   FilterText := CBFilter.Text;

   if CBFilter.Text <> ''
   then
      begin

         Items := TStringList.Create;

         with TIniFile.Create(ChangeFileExt(GetCurrentProjectFileName, '.ini')) do
         try

            StrToStrings(ReadString('LogCatOptions', 'LogCatFilters', GetPackageName), '¤', Items, False);

            Found := False;

            for i := 0 to Items.count - 1 do
               if Items[i] = CBFilter.Text
               then
                  begin
                     Found := True;
                     Break;
                  end;

            if not Found
            then
               begin

                  if Items.count >= 10
                  then
                     Items.Delete(9);

                  if GetPackageName = ''
                  then
                     Items.Insert(0, CBFilter.Text)
                  else
                     Items.Insert(1, CBFilter.Text);

                  WriteString('LogCatOptions', 'LogCatFilters', StringsToStr(Items, '¤'));
                  UpdateFile;

                  CBFilter.Items.Text := StringsToStr(Items, sLineBreak);

               end;

            CBFilter.ItemIndex := CBFilter.Items.IndexOf(FilterText);

         finally
            Free;
         end;

         Items.DisposeOf;

      end;

   RestartMessageLoop;

end;

procedure TFLogCat.CBFilterDropDown(Sender: TObject);

var
   Items: TStringList;

begin

   Items := TStringList.Create;

   with TIniFile.Create(ChangeFileExt(GetCurrentProjectFileName, '.ini')) do
   try

      StrToStrings(ReadString('LogCatOptions', 'LogCatFilters', GetPackageName), '¤', Items);


   finally
      Free;
   end;

   CBFilter.Items.Clear;
   CBFilter.Items.Text := StringsToStr(Items, sLineBreak);

   Items.DisposeOf;

end;

procedure TFLogCat.CBFilterSelect(Sender: TObject);
begin

   BApplyFilterClick(BApplyFilter);;

end;

procedure TFLogCat.CBLogLevelSelect(Sender: TObject);
begin

   RestartMessageLoop;

end;

procedure TFLogCat.CBThisPackClick(Sender: TObject);
begin

   if CBThisPack.Checked
   then
      SavePid := '';

   TBClear.Enabled := False;
   LogCatExpert.StartLogCat(False);

   LogCatExpert.CheckForPid.Enabled := CBThisPack.Checked;

end;

constructor TFLogCat.Create(AOwner: TComponent);
begin

   inherited;

   VSTLogCat.NodeDataSize := SizeOf(rTreeData);
   VSTLogCat.Color := Themeproperties.Background2;

   LCMessages := TList<TMessRec>.Create;

   DeskSection := Name;

   AutoSave := True;

   SaveStateNecessary := True;

end;

destructor TFLogCat.Destroy;
begin

   SaveStateNecessary := True;

   inherited;

end;

procedure TFLogCat.Focus;
begin

end;

procedure TFLogCat.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

   if ActiveControl = CBFilter
   then
      if Key = VK_RETURN
      then
         begin
            BApplyFilterClick(BApplyFilter);
            Key := 0;
         end;

end;

procedure TFLogCat.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

   if ActiveControl = CBFilter
   then
      if Key = VK_RETURN
      then
         Key := 0;

end;

procedure TFLogCat.MIAllToClipBrdClick(Sender: TObject);

var
   i: Integer;
   FileLines: TStringList;

begin

   FileLines := TStringList.Create;

   for i := 0 to LCMessages.Count - 1 do
      if ((FormInstance.CBLogLevel.ItemIndex = 0) or
          ((FormInstance.CBLogLevel.ItemIndex = 1) and
           ((LCMessages[i].LogLvl = 1) or
            (LCMessages[i].LogLvl = 2) or
            (LCMessages[i].LogLvl = 3) or
            (LCMessages[i].LogLvl = 4) or
            (LCMessages[i].LogLvl = 5) or
            (LCMessages[i].LogLvl = 6))) or
          ((FormInstance.CBLogLevel.ItemIndex = 2) and
           ((LCMessages[i].LogLvl = 2) or
            (LCMessages[i].LogLvl = 3) or
            (LCMessages[i].LogLvl = 4) or
            (LCMessages[i].LogLvl = 5) or
            (LCMessages[i].LogLvl = 6))) or
          ((FormInstance.CBLogLevel.ItemIndex = 3) and
           ((LCMessages[i].LogLvl = 3) or
            (LCMessages[i].LogLvl = 4) or
            (LCMessages[i].LogLvl = 5) or
            (LCMessages[i].LogLvl = 6))) or
          ((FormInstance.CBLogLevel.ItemIndex = 4) and
           ((LCMessages[i].LogLvl = 4) or
            (LCMessages[i].LogLvl = 5) or
            (LCMessages[i].LogLvl = 6))) or
          ((FormInstance.CBLogLevel.ItemIndex = 5) and
            ((LCMessages[i].LogLvl = 5) or
            (LCMessages[i].LogLvl = 6)))) and
         ((FilterText = '') or
          (StrFind(StrLower(FilterText), StrLower(LCMessages[i].LCMess)) > 0))
      then
         FileLines.Add(LCMessages[i].LCMess);

   Clipboard.AsText := FileLines.Text;

   FileLines.DisposeOf;

   ShowMessage('Messages has been copied to the clipboard');

end;

procedure TFLogCat.MIMessToClipBrdClick(Sender: TObject);

var
   NodeData: PTreeData;
   Node: PVirtualNode;
   FileLines: TStringList;

begin

   FileLines := TStringList.Create;

   Node :=  VSTLogCat.GetFirstSelected;

   while Node <> nil do
      begin

         NodeData := VSTLogCat.GetNodeData(Node);

         FileLines.Add(LCMessages[NodeData.idx].LCMess);

         Node := VSTLogCat.GetNextSelected(Node);

      end;

   Clipboard.AsText := FileLines.Text;

   FileLines.DisposeOf;

   ShowMessage('Selected messages has been copied to the clipboard');

end;

procedure TFLogCat.MISavetofileClick(Sender: TObject);

var
   i: Integer;
   FileLines: TStringList;

begin

   SDLogCat.InitialDir := ExtractFileDir(GetCurrentProjectFileName);

   if SDLogCat.Execute
   then
      begin

         FileLines := TStringList.Create;

         for i := 0 to LCMessages.Count - 1 do
            if ((FormInstance.CBLogLevel.ItemIndex = 0) or
                ((FormInstance.CBLogLevel.ItemIndex = 1) and
                 ((LCMessages[i].LogLvl = 1) or
                  (LCMessages[i].LogLvl = 2) or
                  (LCMessages[i].LogLvl = 3) or
                  (LCMessages[i].LogLvl = 4) or
                  (LCMessages[i].LogLvl = 5) or
                  (LCMessages[i].LogLvl = 6))) or
                ((FormInstance.CBLogLevel.ItemIndex = 2) and
                 ((LCMessages[i].LogLvl = 2) or
                  (LCMessages[i].LogLvl = 3) or
                  (LCMessages[i].LogLvl = 4) or
                  (LCMessages[i].LogLvl = 5) or
                  (LCMessages[i].LogLvl = 6))) or
                ((FormInstance.CBLogLevel.ItemIndex = 3) and
                 ((LCMessages[i].LogLvl = 3) or
                  (LCMessages[i].LogLvl = 4) or
                  (LCMessages[i].LogLvl = 5) or
                  (LCMessages[i].LogLvl = 6))) or
                ((FormInstance.CBLogLevel.ItemIndex = 4) and
                 ((LCMessages[i].LogLvl = 4) or
                  (LCMessages[i].LogLvl = 5) or
                  (LCMessages[i].LogLvl = 6))) or
                ((FormInstance.CBLogLevel.ItemIndex = 5) and
                  ((LCMessages[i].LogLvl = 5) or
                  (LCMessages[i].LogLvl = 6)))) and
               ((FilterText = '') or
                (StrFind(StrLower(FilterText), StrLower(LCMessages[i].LCMess)) > 0))
            then
               FileLines.Add(LCMessages[i].LCMess);

         FileLines.SaveToFile(SDLogCat.FileName);
         FileLines.DisposeOf;

      end;

end;

class procedure TFLogCat.CreateDockableLogCat;
begin

   If Not Assigned(FormInstance)
   Then
      CreateDockableForm(FormInstance, TFLogCat);

end;

class procedure TFLogCat.RemoveDockableLogCat;
begin

   FreeDockableForm(FormInstance);

end;

class procedure TFLogCat.ShowDockableLogCat;
begin

   CreateDockableLogCat;

   ShowDockableForm(FormInstance);

end;

procedure TFLogCat.TBClearClick(Sender: TObject);
begin

   TBClear.Enabled := False;
   LogCatExpert.StartLogCat(True);

end;

procedure TFLogCat.VSTLogCatDrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);

var
  NodeData: PTreeData;

begin

   DefaultDraw := False;

   NodeData := Sender.GetNodeData(Node);

   if VSTLogCat.Header.Columns[0].Width < TargetCanvas.TextWidth(LCMessages[NodeData.idx].LCMess) + 50
   then
      VSTLogCat.Header.Columns[0].Width := TargetCanvas.TextWidth(LCMessages[NodeData.idx].LCMess) + 50;

   case LCMessages[NodeData.idx].LogLvl of

      0:  TargetCanvas.Font.Color := clWhite;

      1:  TargetCanvas.Font.Color := clGreen;

      2:  TargetCanvas.Font.Color := clYellow;

      3:  TargetCanvas.Font.Color := clAqua;

      4:  TargetCanvas.Font.Color := clRed;

      5:  TargetCanvas.Font.Color := clRed;

   else
      TargetCanvas.Font.Color := clWhite;
   end;

   TargetCanvas.TextOut(CellRect.Left, CellRect.Top, LCMessages[NodeData.idx].LCMess);

end;

procedure TFLogCat.VSTLogCatGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
   NodeDataSize := SizeOf(rTreeData);
end;

procedure TFLogCat.VSTLogCatGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);

var
  NodeData: PTreeData;

begin

   NodeData := Sender.GetNodeData(Node);
   CellText := LCMessages[NodeData.idx].LCMess;

end;

end.


