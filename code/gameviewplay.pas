{
  Copyright 2018-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Display the game map, play the game. }
unit GameViewPlay;

interface

uses Classes,
  CastleControls, CastleTiledMap, CastleUIControls, CastleTransform,
  CastleVectors, CastleKeysMouse, CastleScene, CastleViewport,
  GameUnit, PathfinderUnit,System.Generics.Collections;

Const MAXSPEED=50;  //Tile speed cost, lower means faster
      DragThreshold = 5.0;

type

  TMoveResult = (mrNoMovementTooSmall, mrNoMovementAtTarget, mrMoved);

  TCubeCoord = record
    X, Y, Z: Integer; // x + y + z = 0 always
  end;

  TSelectionOverlay = class(TCastleUserInterface)
  public
    Dragging: Boolean;
    DragStart, DragEnd: TVector2;
    procedure Render; override;
  end;

  TGroupMove=class
      RelativePos:TVector2Integer;
      Delta: TCubeCoord;
      OrigUnit: TUnit;
  end;

  TGroupMoveList=TObjectList<TGroupMove>;

  TViewPlay = class(TCastleView)
  private
    SelectionOverlay :TSelectionOverlay ;
    UnitUnderMouse: TUnit;
    GroupMoveList:TGroupMoveList; //all group units relative to selected
    procedure DoPaintPath;
    procedure ClearMapPath;
    function TryMoveTowards(var PosS: TVector3; const PosT: TVector3; const S,
      SecsPas: Single; out MovedDistance: Single): TMoveResult;
    procedure ClearSelection;
    procedure DoPaintgroup;
    procedure ClearGroupMove;
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    Map: TCastleTiledMap;
    ButtonQuit: TCastleButton;
    ButtonInstructions, ButtonInstructions2: TCastleButton;
    ButtonEndTurn: TCastleButton;
    LabelStatus, LabelTurnStatus: TCastleLabel;
    ViewportMap: TCastleViewport;
  strict private
    TileUnderMouseImage: TCastleImageTransform;
    SelectedUnitVisualization: TCastleTransform;
    TileUnderMouseExists: Boolean;
    TileUnderMouse: TVector2Integer;
    UnitsOnMap: TUnitsOnMap;
    HumanTurn: Boolean;
    SelectedUnit: TUnit;
    procedure ClickQuit(Sender: TObject);
    procedure ClickInstructions(Sender: TObject);
    procedure ClickInstructions2(Sender: TObject);
    procedure ClickEndTurn(Sender: TObject);
    procedure UpdateTurnStatus;
    procedure SelectUnitsInRect(const StartScreen, EndScreen: TVector2);
  public
    { Set this before starting this view. }
    MapName: String;
    DrawPath: TList;
    DrawAtTile: TVector2Integer;
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single;
      var HandleInput: boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
//    procedure Render; override;
    function Release(const Event: TInputPressRelease):Boolean; override;
    function Motion(const Event: TInputMotion):Boolean; override;
    function IsTilePassable(X, Y: Integer): Boolean;
    function getUnitSpeed(X, Y: Integer): Integer;
    function CreateGroupMoveList:boolean;
  end;

var
  ViewPlay: TViewPlay;

implementation

uses SysUtils, windows,math,
  CastleComponentSerialize, CastleUtils, CastleRectangles, CastleColors,
  GameViewMainMenu, GameViewInstructions, GameViewInstructions2,
  GameViewWin, CastleGLUtils, CastleRenderOptions, CastleCameras;

function  CubeCoord(x,y,z:integer):TCubeCoord;
begin
  result.X:=x;result.Y:=y;result.Z:=z;
end;

function OffsetToCube(const T: TVector2Integer): TCubeCoord;
var
  x, y, z: Integer;
begin
  x := T.X - (T.Y + (T.Y and 1)) div 2;
  z := T.Y;
  y := -x - z;
  Result := CubeCoord(x, y, z);
end;

function CubeToOffset(const C: TCubeCoord): TVector2Integer;
var
  col, row: Integer;
begin
  col := C.X + (C.Z + (C.Z and 1)) div 2;
  row := C.Z;
  Result := Vector2Integer(col, row);
end;

// Subtract 2 cube coords
function CubeDelta(const A, B: TCubeCoord): TCubeCoord;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
  Result.Z := A.Z - B.Z;
end;

// Add cube coords
function CubeAdd(const A, B: TCubeCoord): TCubeCoord;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
  Result.Z := A.Z + B.Z;
end;

function TTile2Vector(t:TTile):TVector2Integer;
begin
   result:=Vector2Integer(t.X,t.Y);
end;

function Vector2TTile(v:TVector2Integer):TTile;
begin
  result:=Tile(v.X,v.Y);
end;


constructor TViewPlay.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewplay.castle-user-interface';
end;

function TViewPlay.CreateGroupMoveList:boolean;
Var gm:TGroupMove;
    i:integer;
    SelList:TUnitList;
    GrUnit:tunit;
begin
   result:=false;
   if assigned(GroupMoveList) then
    GroupMoveList.Free;
   if not UnitsOnMap.GroupList.TryGetValue(UnitsOnMap.SelectedGroup,SelList) then exit;

   GroupMoveList:=TGroupMoveList.Create(True);
   for i := 0 to SelList.Count-1 do
   begin
      GrUnit := SelList.Items[i];
      gm:=TGroupMove.Create;
      GroupMoveList.Add(gm);
      gm.OrigUnit:=GrUnit;
      gm.Delta := CubeDelta(OffsetToCube(GrUnit.TilePosition),OffsetToCube(SelectedUnit.TilePosition));
   end;
   result:=true;
end;

procedure TViewPlay.Start;

  procedure PlaceInitialUnits;

    procedure AddUnit(const Kind: TUnitKind; const XBegin, XEnd, YBegin, YEnd: Integer);
    const
      MaxTries = 50;
    var
      I: Integer;
      Pos: TVector2Integer;
      Un: TUnit;
    begin
      { Choose a random position, within given X and Y ranges.
        Since we may play on various map sizes, with various rivers
        --- prepare that the units may not fit, and resign after MaxTries
        from adding a new unit. }
      for I := 1 to MaxTries do
      begin
        Pos.X := RandomIntRangeInclusive(XBegin, XEnd);
        Pos.Y := RandomIntRangeInclusive(YBegin, YEnd);
        if (UnitsOnMap[Pos] = nil) and
           (not UnitsOnMap.IsWater(Pos)) then
        begin
          Un := TUnit.Create(FreeAtStop);
          Un.TilePosition := Pos;
          Un.Initialize(UnitsOnMap, Kind);
          Map.Add(Un.Transform);
          Exit;
        end;
      end;
    end;

  var
    I, W, H, YBegin, YEnd: Integer;
  begin
    W := Map.Data.Width;
    H := Map.Data.Height;
    YBegin := H div 3;
    YEnd := H - 1 - H div 3;
    AddUnit(ukHoplites, 0, 0, 0, 0);

    for I := 0 to 2 do
      AddUnit(ukAlienHeavy, W div 8 + 0, W div 8 + 0, YBegin, YEnd);
    for I := 0 to 3 do
      AddUnit(ukAlienLight, W div 8 + 2, W div 8 + 2, YBegin, YEnd);
    for I := 0 to 2 do
      AddUnit(ukHoplites, W - 1 - W div 8 - 0, W - 1 - W div 8 - 0, YBegin, YEnd);
    for I := 0 to 3 do
      AddUnit(ukArchers, W - 1 - W div 8 - 2, W - 1 - W div 8 - 2, YBegin, YEnd);
    for I := 0 to 3 do
      AddUnit(ukKnights, W - 1 - W div 8 - 3, W - 1 - W div 8 - 1, YBegin, YEnd);



  end;

begin
  inherited;

  Map.URL := 'castle-data:/maps/' + MapName + '.tmx';
  ButtonQuit.OnClick := {$ifdef FPC}@{$endif}ClickQuit;
  ButtonInstructions.OnClick := {$ifdef FPC}@{$endif}ClickInstructions;
  ButtonInstructions2.OnClick := {$ifdef FPC}@{$endif}ClickInstructions2;
  ButtonEndTurn.OnClick := {$ifdef FPC}@{$endif}ClickEndTurn;

  UnitsOnMap := TUnitsOnMap.Create(FreeAtStop, Map);

  PlaceInitialUnits;



  TileUnderMouseImage := TCastleImageTransform.Create(FreeAtStop);
  case Map.Data.Orientation of
    moOrthogonal: TileUnderMouseImage.URL := 'castle-data:/tile_hover/orthogonal.png';
    moHexagonal : TileUnderMouseImage.URL := 'castle-data:/tile_hover/hexagonal.png';
    else          TileUnderMouseImage.URL := 'castle-data:/tile_hover/isometric.png';
  end;
  TileUnderMouseImage.Exists := false;
  Map.Add(TileUnderMouseImage);

  SelectedUnitVisualization := TransformLoad(
    'castle-data:/unit_selected.castle-transform', FreeAtStop);
  SelectedUnitVisualization.Exists := false;
  Map.Add(SelectedUnitVisualization);

  HumanTurn := true;
  UpdateTurnStatus;


  ViewportMap.camera.Translation:=UnitsOnMap.Units[9].Transform.Position-Vector3(1400,200,0);//Vector3(1200,500,100);
  ViewportMap.camera.Orthographic.Height:=1500;

  (Viewplay.ViewportMap.Navigation as TCastle2DNavigation).MouseButtonMove:=buttonRight;
  SelectionOverlay := TSelectionOverlay.Create(FreeAtStop);
  SelectionOverlay.FullSize := true;
  SelectionOverlay.Exists:=true;
  InsertFront(SelectionOverlay); // ensure it draws on top

end;

procedure TViewPlay.Stop;
begin
  { Make sure to clear fields, otherwise stopping + starting this view again
    (when you exit the game and start a new game) could have non-nil but
    invalid SelectedUnit reference. }
  SelectedUnit := nil;
  TileUnderMouseExists := false;
  // set to nil, as it will be freed by FreeAtStop
  SelectedUnitVisualization := nil;
  inherited;
end;

procedure TViewPlay.ClickQuit(Sender: TObject);
begin
  Container.View := ViewMainMenu;
end;

procedure TViewPlay.ClickInstructions(Sender: TObject);
begin
  Container.PushView(ViewInstructions);
end;

procedure TViewPlay.ClickInstructions2(Sender: TObject);
begin
  Container.PushView(ViewInstructions2);
end;

procedure TViewPlay.ClickEndTurn(Sender: TObject);

  procedure ResetMovement;
  var
    I: Integer;
  begin
    for I := 0 to UnitsOnMap.UnitsCount - 1 do
      UnitsOnMap.Units[I].Movement := UnitsOnMap.Units[I].InitialMovement;
  end;

begin
  HumanTurn := not HumanTurn;
  SelectedUnit := nil;
  SelectedUnitVisualization.Exists := false;
  ResetMovement;
  UpdateTurnStatus;
end;



function TViewPlay.Press(const Event: TInputPressRelease): Boolean;

  procedure CheckWin;
  var
    FoundEnemy: Boolean;
    I: Integer;
  begin
    FoundEnemy := false;
    for I := 0 to UnitsOnMap.UnitsCount - 1 do
      if UnitsOnMap.Units[I].Human <> HumanTurn then
      begin
        FoundEnemy := true;
        Break;
      end;

    if not FoundEnemy then
    begin
      ViewWin.HumansWin := HumanTurn;
      Container.PushView(ViewWin);
    end;
  end;

  procedure ShowSelectedUnit;
  begin
    SelectedUnitVisualization.Exists := true;
    SelectedUnitVisualization.Parent := SelectedUnit.Transform;
    SelectedUnitVisualization.Rotation := SelectedUnit.FaceRotation;
    //If in group select all group
    if SelectedUnit.InGroup then
     UnitsOnMap.SelectedGroup:=SelectedUnit.GroupID;
  end;

var Gid:integer;
begin
  Result := inherited;
  if Result then Exit;
  if SelectionOverlay.Dragging then
  begin
     exit;
  end;

  if (mkCtrl in Event.ModifiersDown) and (event.Key in [key0..key9]) then   //ctrl1- ctrl9 add groups
  begin
    Gid:=ord(event.Key)-48;
    UnitsOnMap.DeleteGroup(Gid);//destroys group - empty list from units
    UnitsOnMap.AddSelectedUnitsToGroup(Gid);//add all selected nunits to this group
    exit(true);
  end
  else
   if event.Key in [key0..key9] then
   Begin //select group
      Gid:=ord(event.Key)-48;
      ClearSelection;
      UnitsOnMap.SelectedGroup:=Gid;
      exit(true);
   end;


  if Event.IsMouseButton(buttonLeft) and TileUnderMouseExists then
  begin
     //SelectionOverlay.Dragging := True;
    SelectionOverlay.DragStart := Event.Position;
    SelectionOverlay.DragEnd := SelectionOverlay.DragStart;
    UnitUnderMouse := UnitsOnMap[TileUnderMouse];
    if (UnitUnderMouse <> nil) and (UnitUnderMouse.Human = HumanTurn) then
    begin
      if (SelectedUnit <> nil) and (SelectedUnit=UnitUnderMouse) then //we clicked again
      begin
        SelectedUnit.UnitFacing := SelectedUnit.UnitFacing + 1;
      end;
      // select new unit
      SelectedUnit := UnitUnderMouse;
      ShowSelectedUnit;
      UpdateTurnStatus; // SelectedUnit changed
      Exit(true); // event handled
    end else
    if (SelectedUnit <> nil) and
       // CanMove also checks that SelectedUnit.Movement > 0
       (SelectedUnit.CanMove(TileUnderMouse)) then
    begin
      if UnitUnderMouse <> nil then
      begin
        // hurt enemy UnitUnderMouse.
        UnitUnderMouse.Life := UnitUnderMouse.Life - SelectedUnit.Attack;
        // Above operation *maybe* freed and removed enemy from the map,
        // so UnitUnderMouse pointer afterwards is no longer valid.
        UnitUnderMouse := nil;
        SelectedUnit.Movement := 0;
        UpdateTurnStatus; // SelectedUnit stats changed
        CheckWin;
      end else
      begin
        // move
        SelectedUnit.MovingPath := DrawPath;
        //todo:if in group Move all Units
        UpdateTurnStatus; // SelectedUnit stats changed
        SelectedUnit:=nil;
        SelectedUnitVisualization.Exists := false;
        ClearMapPath;
        ClearGroupMove;
      end;
      Exit(true); // event handled
    end;

    { When clicking on other map tile, do not mark event as handled
      by Exit(true) here.
      This allows TCastle2DNavigation to handle clicks to pan the map. }
  end
  else if Event.IsMouseButton(buttonRight) then
  begin
    ClearSelection;
    SelectedUnit:=nil;
    SelectedUnitVisualization.Exists := false;
    ClearMapPath;
    ClearGroupMove;
    //Result:=true;
  end;
end;

function TViewPlay.Release(const Event: TInputPressRelease):boolean;
begin
  Result:=inherited;
  if Event.IsMouseButton(buttonLeft) and SelectionOverlay.Dragging then
  begin
    SelectionOverlay.Dragging := False;
    SelectionOverlay.DragEnd := Event.Position;

  // Now perform unit selection
    SelectUnitsInRect(SelectionOverlay.DragStart, SelectionOverlay.DragEnd);
    Result:=true;
  end;
end;

function TViewPlay.Motion(const Event: TInputMotion):boolean;
var
  DragDistance: Single;
begin
  Result:=Inherited;

  if buttonLeft in Event.Pressed then
  begin
    DragDistance := (Event.Position - SelectionOverlay.DragStart).Length;

    if  (DragDistance > DragThreshold) then
    begin
      // Start selection
      SelectionOverlay.Dragging := true;
      ClearSelection;//clear previous selected units
    end;
    if SelectionOverlay.Dragging then
    begin
      if not TVector2.Equals(SelectionOverlay.DragEnd,Event.Position) then
      begin //
        //Deselect all units
        SelectUnitsInRect(SelectionOverlay.DragStart,Event.Position);
      end;
      SelectionOverlay.DragEnd := Event.Position;
      Result:=True;
    end;
  end;
end;


procedure TViewPlay.ClearSelection;
begin
  UnitsOnMap.ClearSelectedUntis;
end;

procedure TViewPlay.SelectUnitsInRect(const StartScreen, EndScreen: TVector2);
var
  SelectionRect: TFloatRectangle;
  UnitWorldPos, UnitLocalPos : TVector3;
  UnitContainerPos:TVector2;
  myUnit: TUnit; // your custom unit class
  i:integer;
  RayDirection: TVector3;
  vpStart,vpEnd:TVector3;
  sx,sy,ex,ey:Single;

  function SelectionContainsUnit:Boolean;
  begin
    Result:= (sx<UnitWorldPos.X) and (UnitWorldPos.X<ex) and
       (sy<UnitWorldPos.Y) and (UnitWorldPos.Y<ey);
  end;

begin

  ViewportMap.PositionToRay(StartScreen, true, vpStart, RayDirection);
  ViewportMap.PositionToRay(EndScreen, true, vpEnd, RayDirection);
  sx:=min(vpStart.x,vpEnd.x);
  ex:=max(vpStart.x,vpEnd.x);
  sy:=min(vpStart.y,vpEnd.y);
  ey:=max(vpStart.y,vpEnd.y);


  for i:=0 to UnitsOnMap.UnitsCount-1 do
  begin
    myUnit := UnitsOnMap.Units[i];
    UnitWorldPos := myUnit.Transform.Translation; // or Unit.TileToWorld(...) if needed
    myUnit.Selected :=SelectionContainsUnit;
  end;
end;


procedure TViewPlay.UpdateTurnStatus;
var
  SideName: String;
begin
  if HumanTurn then
    SideName := 'Human'
  else
    SideName := 'Alien';
  LabelTurnStatus.Caption := SideName + ' Turn';
  LabelTurnStatus.Text.Add(''); // newline

  if SelectedUnit <> nil then
  begin
    LabelTurnStatus.Text.Add('Selected: ' + SelectedUnit.ToString);
    LabelTurnStatus.Text.Add('Move or attack');
    LabelTurnStatus.Text.Add('  or select another ' + SideName + ' unit');
    LabelTurnStatus.Text.Add('  or press "End Turn".');
  end else
  begin
    LabelTurnStatus.Text.Add('No unit selected.');
    LabelTurnStatus.Text.Add('Select ' + SideName + ' unit');
    LabelTurnStatus.Text.Add('  or press "End Turn".');
  end;
end;



  {go from posS to posT at S pixels per second}
function TViewPlay.TryMoveTowards(var PosS: TVector3; const PosT: TVector3;
  const S: Single; const SecsPas: Single; out MovedDistance: Single): TMoveResult;
var
  direction: TVector3;
  speedPerSecond, finalSpeed: Single;
  distance: Single;
  TileSpeed:Single;
  TileTarget:TVector2Integer;
begin
  Map.Data.PositionToTile(PosT.XY, TileTarget);
  //TileSpeed := (0 - (getUnitSpeed(TileTarget.X,TileTarget.Y) - MAXSPEED) ) /50 ;
  speedPerSecond := S ;
  TileSpeed :=  getUnitSpeed(TileTarget.X,TileTarget.Y) /MAXSPEED ;

  finalSpeed := speedPerSecond - speedPerSecond * TileSpeed; //MAXSPEED is max cost speed
  direction := PosT - PosS;

  if direction.Length <= 1 then
  begin
    MovedDistance := 0;
    Result := mrNoMovementAtTarget; // Already at destination
  end
  else
  begin
    direction := direction.Normalize;
    distance := finalSpeed * SecsPas;

    if distance < 0.001 then
    begin
      MovedDistance := 0;
      Result := mrNoMovementTooSmall; // Too little to move
    end
    else
    begin
      PosS := PosS + direction * distance;
      MovedDistance := distance;
      Result := mrMoved; // Actual movement
    end;
  end;
end;

function TViewPlay.IsTilePassable(X, Y: Integer): Boolean;
begin

  Result:= not UnitsOnMap.IsWater(Vector2Integer(X,Y));
end;

//returns the tilespeed rename this
function TViewPlay.getUnitSpeed(X, Y: Integer): Integer;
var
  TilePosition : TVector2Integer;
  Tileset: TCastleTiledMapData.TTileset;
  Frame: Integer;
  HorizontalFlip, VerticalFlip, DiagonalFlip: Boolean;
  FrameSpeed:integer;
begin
   if not IsTilePassable(X,Y) then
    exit(0);

   TilePosition := Vector2Integer(X,Y);
   Map.Data.TileRenderData(TilePosition,
    Map.Data.Layers[0],
    Tileset, Frame, HorizontalFlip, VerticalFlip, DiagonalFlip);

  Case Frame of //tiles  the bigger the speed the less passable something is
  //if framespeed=0 then we have maxspeed
    0: FrameSpeed := 10;
    1: FrameSpeed := 20;
    2: FrameSpeed := 20;
    3: FrameSpeed := 30;
    4: FrameSpeed := 40;
    5: FrameSpeed := MAXSPEED;  //MAXSPEED = no passable  real speed goes to 0
    6: FrameSpeed := MAXSPEED;
    7: FrameSpeed := MAXSPEED;
    8: FrameSpeed := 40;
    9: FrameSpeed := 30;
    10: FrameSpeed := MAXSPEED;
    11: FrameSpeed := MAXSPEED;
  End;

  Result:= FrameSpeed;

end;

procedure TViewPlay.ClearGroupMove;
var i:integer;
begin
       for i := map.Count-1 downto  0 do
        begin
          if TCastleTransform(map.Items[i]).Tag=8 then
           map.Remove(TCastleTransform(map.Items[i]));
        end;
end;

procedure TViewPlay.DoPaintgroup;
var i:integer;
    grm:TGroupMove;
    TileVector:TVector2Integer;

    procedure addUnitPlaceHolder;
    var
         TileRect: TFloatRectangle;
         TileImage : TCastleImageTransform;
    begin
       // s:=s+'('+inttostr(t.X)+','+inttostr(t.Y)+')->';
       TileRect := Map.TileRectangle(TileVector);
       TileImage := TCastleImageTransform.Create(FreeAtStop);
       TileImage.URL := 'castle-data:/tile_hover/hexagonal.png';
       TileImage.Translation := Vector3(TileRect.Center, ZHover);
       TileImage.Size := Vector2(TileRect.Width, TileRect.Height);
       TileImage.Tag:=8;
       if (grm.OrigUnit<>SelectedUnit) and UnitsOnMap.IsWater(TileVector) then     //check if tile is valid
        TileImage.Color:=HexToColor('FF3333')
       else if (grm.OrigUnit<>SelectedUnit) then
         TileImage.Color:=HexToColor('66FF66')
       else
         TileImage.Color:=HexToColor('3399FF');
       TileImage.Exists := true;
       Map.Add(TileImage);
    end;

var t:TTile;
    selTarg:TVector2Integer;
    selTargTile:TTile;

begin
   ClearGroupMove;
   //selTarg is the last tile of drawpath
   selTargTile:= PathfinderUnit.TTile(DrawPath.First^);
   selTarg:= TTile2Vector(selTargTile);
   if CreateGroupMoveList then
   begin
     for i:=0 to GroupMoveList.Count-1 do
     Begin
       grm:=GroupMoveList[i];
       TileVector:=CubeToOffset(CubeAdd(OffsetToCube(selTarg),grm.Delta));
       addUnitPlaceHolder;
     End;
   end;
end;


procedure TViewPlay.DoPaintPath;
var
  I: Integer;
  t: PathfinderUnit.TTile;
  TileVector:TVector2Integer;
  TileRect: TFloatRectangle;
  TileImage : TCastleImageTransform;
  //s:String;
Begin
  //exit;
  //s:='';
  for I := 0 to DrawPath.Count -1 do
  begin
    t:= PathfinderUnit.TTile(DrawPath.Items[i]^);
    TileVector:= Vector2Integer(t.X,t.Y);
   // s:=s+'('+inttostr(t.X)+','+inttostr(t.Y)+')->';
    TileRect := Map.TileRectangle(TileVector);
    TileImage := TCastleImageTransform.Create(FreeAtStop);
    TileImage.URL := 'castle-data:/tile_hover/hexagonal.png';
    TileImage.Translation := Vector3(TileRect.Center, ZHover);
    TileImage.Size := Vector2(TileRect.Width, TileRect.Height);
    TileImage.Tag:=9;
    Map.Add(TileImage);
    TileImage.Exists := true;
   // Outputdebugstring(PChar(s));
  end;

End;


procedure TViewPlay.ClearMapPath;
var i:integer;
begin
       for i := map.Count-1 downto  0 do
        begin
          if TCastleTransform(map.Items[i]).Tag=9 then
           map.Remove(TCastleTransform(map.Items[i]));
        end;
end;

procedure TViewPlay.Update(const SecondsPassed: Single;
  var HandleInput: boolean);
const
  HoverAlpha = 0.75;
  { When choosing colors, be sure to choose something clearly visible on our
    tiles background. Light green, or yellow. or light blue -> not good,
    as would mix with some map tiles. }
  ColorNotAllowed: TCastleColor = (X: 1; Y: 0; Z: 0; W: HoverAlpha); // red
  ColorAllowed: TCastleColor = (X: 0; Y: 1; Z: 0; W: HoverAlpha); // green
  ColorMoveAllowed: TCastleColor = (X: 1; Y: 1; Z: 1; W: HoverAlpha); // white
var
  TileStr: String;
  TileRect: TFloatRectangle;
  UnitUnderMouse: TUnit;
  RayOrigin, RayDirection: TVector3;
  //MapIndex: Integer;
  mvUnit:TUnit;
  strtPos,targPos:TVector3;
  mdist:Single;
  i:integer;
  R: TFloatRectangle;
  pathf : TPathfinder;
  tempUList:TunitList;
begin
  if SelectionOverlay.Dragging then
  begin
    TileUnderMouseImage.Exists:=false;
    exit;
  end;


  ViewportMap.PositionToRay(Container.MousePosition, true, RayOrigin, RayDirection);

  { Update TileUnderMouseExists, TileUnderMouse.
    See https://castle-engine.io/tiled_maps#tile_under_mouse for an explanation
    how to query the map tile under mouse. }
  TileUnderMouseExists := Map.Data.PositionToTile(RayOrigin.XY, TileUnderMouse);

  { This is alternative way to query the map tile under mouse,
    that will work even if map is possibly transformed,
    directly or by parent TCastleTransform,
    or if you may have different camera direction, maybe even 3D with perspective.

  TileUnderMouseExists := false;
  if ViewportMap.MouseRayHit <> nil then
  begin
    MapIndex := ViewportMap.MouseRayHit.IndexOfItem(Map);
    if MapIndex <> -1 then
    begin
      TileUnderMouseExists := Map.Data.PositionToTile(
        ViewportMap.MouseRayHit[MapIndex].Point.XY, TileUnderMouse);
    end;
  end;
  }

  { update TileUnderMouseImage, UnitUnderMouse }
  TileUnderMouseImage.Exists := TileUnderMouseExists;
  if TileUnderMouseExists then
  begin
    TileRect := Map.TileRectangle(TileUnderMouse);
    TileUnderMouseImage.Translation := Vector3(TileRect.Center, ZHover);
    TileUnderMouseImage.Size := Vector2(TileRect.Width, TileRect.Height);
    UnitUnderMouse := UnitsOnMap[TileUnderMouse];
    //paint path to tileundermouse
    if (SelectedUnit <> nil) and not TVector2Integer.Equals(DrawAtTile,TileUnderMouse) and not TVector2Integer.Equals(SelectedUnit.TilePosition,TileUnderMouse) then
    begin
       pathf := TPathfinder.Create(Selectedunit.TilePosition.X,Selectedunit.TilePosition.Y, TileUnderMouse.X, TileUnderMouse.Y);
       pathf.OnIsTilePassable := IsTilePassable;
       pathf.OnGetSpeed := GetUnitSpeed;
       ClearMapPath;
       ClearGroupMove;
       DrawPath := pathf.FindPath;  //list of ttile Records
       if assigned(DrawPath) and (DrawPath.Count>0) then
       begin
        DrawAtTile :=  TileUnderMouse;
        DoPaintPath;
        if SelectedUnit.InGroup then
          DoPaintgroup;
       end
       else DrawAtTile:= TVector2Integer.Zero;
       pathf.Free;
    end;
  end else
    UnitUnderMouse := nil;



  { update TileUnderMouseImage.Color }
  if (SelectedUnit <> nil) and
     SelectedUnit.CanMove(TileUnderMouse) then
    // can move by clicking here
    TileUnderMouseImage.Color := ColorMoveAllowed
  else
  if (UnitUnderMouse <> nil) and
     (UnitUnderMouse.Human = HumanTurn) then
    // can select by clicking here
    TileUnderMouseImage.Color := ColorAllowed
  else
    // cannot do anything by clicking here
    TileUnderMouseImage.Color := ColorNotAllowed;

  { update LabelStatus }
  if TileUnderMouseExists then
  begin
    TileStr := TileUnderMouse.ToString;
    if UnitsOnMap.IsWater(TileUnderMouse) then
      TileStr := TileStr + NL + ' Water';
    if UnitUnderMouse <> nil then
      TileStr := TileStr + NL + ' Unit: ' + UnitUnderMouse.ToString;
  end else
    TileStr := 'none';
  LabelStatus.Caption := Format('FPS: %s' + NL + 'Tile: %s', [
    Container.Fps.ToString,
    TileStr
  ]);

  {Update moving units}
  tempUList:=TunitList.Create(False);
  try
  for I := 0 to UnitsOnMap.UnitsCount - 1 do
  begin
     try
      mvUnit:= UnitsOnMap.Units[I];
     except
        break;
     end;
      if not TVector2Integer.equals(mvUnit.TargetTile,mvUnit.TilePosition) then
      begin //unit is moving
        mvUnit.SecondsPassed:=mvUnit.SecondsPassed+SecondsPassed;

        R := UnitsOnMap.Map.TileRectangle(mvUnit.TargetTile);
        targPos := Vector3(R.Center, ZUnit);
        strtPos := mvUnit.Transform.Translation;
        case TryMoveTowards(strtPos,targPos,mvUnit.Speed,mvUnit.SecondsPassed,mdist) of
          mrNoMovementTooSmall: ; //nothing todo we already added the secs
          mrNoMovementAtTarget: tempUList.Add(mvUnit); //add unit to move ended list

          mrMoved: begin
                     mvUnit.SecondsPassed:=0;
                     mvUnit.Transform.Translation:=strtPos;
                   end;
         end;

      end;
  end;
   for mvUnit in tempUList do
   Begin
      mvUnit.TilePosition:=mvUnit.TargetTile;
      mvUnit.PathReached;
   End;
  finally
   tempUList.Free;
  end;


end;

{ TSelectionOverlay }

procedure TSelectionOverlay.Render;
Const BorderWidth=4;
var
  LeftX, RightX, TopY, BottomY: Single;
  FillColor, BorderColor: TCastleColor;
  R: TFloatRectangle;

begin
  inherited;

  if Dragging then
  begin

    LeftX := Min(DragStart.X, DragEnd.X);
    RightX := Max(DragStart.X, DragEnd.X);
    BottomY := Min(DragStart.Y, DragEnd.Y);
    TopY := Max(DragStart.Y, DragEnd.Y);

    R := FloatRectangle(LeftX, BottomY, RightX - LeftX, TopY - BottomY);
    FillColor := Vector4(1.0, 1.0, 0.0, 0.5);    // Yellow with 50% transparency
    BorderColor := Vector4(1.0, 0.0, 0.0, 1.0);  // Fully opaque yellow

    DrawRectangle(R,FillColor, bsSrcAlpha, bdOneMinusSrcAlpha, true); // <- this means "filled"

    // Optional: also draw outline
//    DrawRectangle(R,BorderColor,bsSrcAlpha, bdOneMinusSrcAlpha, false );

    // Top
    DrawRectangle(FloatRectangle(R.Left, R.Top - BorderWidth, R.Width, BorderWidth), BorderColor, bsSrcAlpha, bdOneMinusSrcAlpha, true);
    // Bottom
    DrawRectangle(FloatRectangle(R.Left, R.Bottom, R.Width, BorderWidth), BorderColor, bsSrcAlpha, bdOneMinusSrcAlpha, true);
    // Left
    DrawRectangle(FloatRectangle(R.Left, R.Bottom, BorderWidth, R.Height), BorderColor, bsSrcAlpha, bdOneMinusSrcAlpha, true);
    // Right
    DrawRectangle(FloatRectangle(R.Right - BorderWidth, R.Bottom, BorderWidth, R.Height), BorderColor, bsSrcAlpha, bdOneMinusSrcAlpha, true);

  end;
end;

end.
