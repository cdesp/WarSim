{
  Copyright 2018-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Unit (soldier) on a map. }
unit GameUnit;

interface

uses Classes, Contnrs,
  CastleTransform, CastleComponentSerialize, CastleVectors, CastleScene,
  CastleTiledMap,Generics.Defaults,System.Generics.Collections,PathfinderUnit,CastleColors,CastleBehaviors ;

type


  TUnitKind = (ukAlienLight, ukAlienHeavy, ukHoplites, ukArchers, ukKnights);

  TUnit = class;
  TUnitList = TObjectList<TUnit>;
  TGroupList = TObjectDictionary<integer,TUnitList>;

  TGroupMove=class
      RelativePos:TVector2Integer;
      Delta: TCubeCoord;
      OrigUnit: TUnit;
      TargTile: TVector2Integer;
   end;

  TGroupMoveList=TObjectList<TGroupMove>;

  TFloatingTextNode = class(TCastleTransform)
  private
    TextNode: TCastleText;
    TimeAlive, Lifetime, Speed: Single;
    UnitFacing: Integer;
    UnitTilePosition: TVector2Integer;
    Color:TCastleColor;
  public
    constructor Create(AOwner: TComponent; const AText: String; AColor: TCastleColor; ALifetime, ASpeed: Single; AUnitFacing: Integer); reintroduce;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  end;


  TUnitsOnMap = class(TComponent)
  private
    FItems: array of array of TUnitList;
    FUnits: TComponentList;
    FGroupList:TGroupList;
    FSelectedGroup:integer;
    UnitID:Integer;
    fGroupMoveList: TGroupMoveList;
    procedure GroupSelect(GroupID: integer);
    procedure GroupUnSelect(GroupID: integer);
    procedure setSelectedGroup(const Value: integer);
    procedure setGroupSelect(GroupID:Integer; V: Boolean);
    function IsValidPosition(const TilePosition: TVector2Integer): boolean;
    procedure SortGroupMoveList(var AGroupMoveList: TGroupMoveList);
  strict private
    FMap: TCastleTiledMap;
    FUnitsCount: Integer;
    function GetUnitOnMap(const TilePosition: TVector2Integer): TUnit;
    function GetUnits(const Index: Integer): TUnit;
    procedure SetUnitsCount(const AValue: Integer);
  public
    property GroupMoveList:TGroupMoveList read fGroupMoveList; //all group units relative to selected
    constructor Create(const AOwner: TComponent;
      const AMap: TCastleTiledMap); reintroduce;
    destructor Destroy; override;

    { What unit is present at each map tile (@nil if none).

      You cannot change it directly, instead
      - add new TUnit,
      - destroy a TUnit,
      - change TUnit.TilePosition.

      In other words, changes to TUnit are automatically reflected here. }
    property Items[const TilePosition: TVector2Integer]: TUnit read GetUnitOnMap; default;

    function TileOccupied(tile:TVector2Integer):boolean;
    function UnitsCount: Integer;
    function UnitsOnTile(const TilePosition: TVector2Integer):TUnitList;
    function NewGroup(GID:Integer):TUnitList;
    procedure ClearSelectedUntis;
    procedure DeleteGroup(GroupID:integer);
    Procedure AddSelectedUnitsToGroup(GroupId:Integer);
    property Units[const Index: Integer]: TUnit read GetUnits;
    property Map: TCastleTiledMap read FMap;
    property GroupList:TGroupList read FGroupList;
    property SelectedGroup:integer read FSelectedGroup write setSelectedGroup;
    function TileIsBlocked(const TilePosition: TVector2Integer;
      IncUnits: boolean=True): boolean;
    function IsTilePassable(const TilePosition: TVector2Integer): Boolean;overload;
    function IsTilePassable(X, Y: Integer): Boolean;overload;
    function CreateGroupMoveList(SelectedUnit:TUnit): boolean;
    procedure MoveGroup;
    procedure SetGroupFacing(Gid, UFace: Integer);

    function getUnitSpeed(X, Y: Integer): Integer;
    function FindAPath(StartTile,TargTile:TVector2Integer):TList;
    function GetNextId:integer;
  end;

  TUnit = class(TComponent)
  private
    class var
      TransformTemplate: TCastleComponentFactory;
    procedure SetMovingPath(const Value: TList);
    function getFacingToTarget: Integer;
    function IsInGroup: Boolean;
    function getGroupID: Integer;
    procedure setGroupID(const Value: Integer);
    procedure setSelected(const Value: Boolean);
    function GetSpeed: Integer;
    procedure SetSpeedGroup(const Value: Integer);
    procedure SetUnits(const Value: Integer);
    procedure SetHuman(const Value: Boolean);
    procedure SetBattleWith(const Value: TUnit);
    function getAttackPower: integer;
    function getDefensePower: integer;
    function getFacingRatio: Single;
    procedure SetCasualties(const Value: Integer);
    procedure SetRange(const Value: Integer);
    procedure CreateCasualtiesAnimation;
    procedure ClearMovingPath;
    function GetPathTarget: TVector2Integer;
  var
      FBattleWith: TUnit;
      FSecondsPassed: Single;
      FSpeed: Integer;
      FSpeedGroup: Integer;
      FTargetTile: TVector2Integer;
      FUnitFacing: Integer;
      FUnitFacingSave: Integer;
      FGroupID: Integer;
      FCasualties:Integer;
      FID: Integer;
      procedure SetSecondsPassed(const Value: Single);
      procedure SetSpeed(const Value: Integer);
      procedure SetTargetTile(const Value: TVector2Integer);
      procedure SetUnitFacing(const Value: Integer);
  strict private
    var
      FKind: TUnitKind;
      FAttack: Integer;
      FUnits: Integer;
      FDefense: Integer;
      FRange: Integer;
      ImageIcon: TCastleImageTransform;
      TextBattle: TCastleText;
      TextAttack: TCastleText;
      TextLife: TCastleText;
      TextMovement: TCastleText;
      Background: TCastleImageTransform;
      SelBox: TCastleBox;
      FTilePosition: TVector2Integer;
      FUnitsOnMap: TUnitsOnMap;
      FMovingPath : TList;
      FSelected: Boolean;
      FHuman: Boolean;
    procedure SetTilePosition(const Value: TVector2Integer);
    procedure SetUnitsOnMap(const Value: TUnitsOnMap);
    procedure PlaceOnMap;
    procedure RemoveFromMap;
    property UnitsOnMap: TUnitsOnMap read FUnitsOnMap write SetUnitsOnMap;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    Transform: TCastleTransform;
    FaceRotation: TVector4;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Initialize(const AUnitsOnMap: TUnitsOnMap;
      const AKind: TUnitKind; const AnAttack, AnDefense, AUnits: Integer); overload;
    { Initialize, choosing default statistics for this TUnitKind. }
    procedure Initialize(const AUnitsOnMap: TUnitsOnMap;
      const AKind: TUnitKind); overload;
    function ToString: String; override;

    { Can this unit move to given new position.
      Returns @true if the tile is empty and possible to walk into (not water),
      or occupied by the enemy (in which case we do attack).}
    function CanMove(const NewTilePosition: TVector2Integer): Boolean;
    procedure PathReached;  //if destination reached
    procedure ClearGroup;  //remove unit from group
    function GroupUnitList:TUnitlist;
    function OnBattle: boolean;
    function IsMoving: boolean;
    function HasMovement: boolean;
    procedure DoBattle;
    function EndBattle: boolean;
    procedure checkBattleNearTile;
    function TileOccupied(Tile:TVector2Integer):boolean;
    function IsOtherUnitHere: Boolean;
    procedure checkValidTile;

    property SecondsPassed: Single read FSecondsPassed write SetSecondsPassed;
    property TargetTile: TVector2Integer read FTargetTile write SetTargetTile;
    property MovingPath:TList read FMovingPath write SetMovingPath;
    property PathTarget: TVector2Integer read GetPathTarget;
    property InGroup:Boolean read IsInGroup;
    property SpeedGroup:Integer read FSpeedGroup write SetSpeedGroup; //the speed of the group
    //======== Battle Properties
    property BattleWith:TUnit read FBattleWith write SetBattleWith; //The enemy ti fight
    property Casualties:Integer read FCasualties write SetCasualties;
    property FacingRatio:Single read getFacingRatio;
    property AttackPower:Integer read getAttackPower;
    property DefensePower:Integer read getDefensePower;
    //=========  Map Properties
    property TilePosition: TVector2Integer read FTilePosition write SetTilePosition;
    property UnitFacing: Integer read FUnitFacing write SetUnitFacing;
    property GroupID:Integer read getGroupID write setGroupID;
    property Selected:Boolean read FSelected write setSelected;
    //========== Main Properties ============
    { You can change units . Setting Units to <= 0 frees the unit instance,
      removing it also from the map. }
    property Units:Integer read FUnits write SetUnits; //the number of troops in the unit
    property Human:Boolean read FHuman write SetHuman;
    property Kind: TUnitKind read FKind;    //unit type
    property Attack: Integer read FAttack;  //the attack of each unit
    property Defense: Integer read FDefense; //the defense of each unit
    property Speed: Integer read GetSpeed write SetSpeed;
    Property Range: Integer read FRange write SetRange; // how far can we attack
    Property ID:Integer read FID;
  end;

  function CubeToOffset(const C: TCubeCoord): TVector2Integer;forward;
  function CubeAdd(const A, B: TCubeCoord): TCubeCoord;forward;
  function OffsetToCube(const T: TVector2Integer): TCubeCoord;forward;

const
  ZUnit = 300.0;
  ZHover = 200.0;

implementation

uses SysUtils, TypInfo, Math, windows,GameViewPlay, System.Types,
  CastleRectangles, CastleStringUtils,  CastleViewport;

{Functions -------------}

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

//============TFloatingTextNode===========================



constructor TFloatingTextNode.Create(AOwner: TComponent; const AText: String; AColor: TCastleColor; ALifetime, ASpeed: Single; AUnitFacing: Integer);
begin
  inherited Create(AOwner);
  Lifetime := ALifetime;
  Speed := ASpeed;
  TimeAlive := 0;
  UnitFacing := AUnitFacing;
  Color := AColor;
  //UnitTilePosition := AUnitTilePosition;

  TextNode := TCastleText.Create(Self);
  TextNode.Text.Add(AText);
  TextNode.Color := AColor;
  TextNode.Size := 30;
 // TextNode.Alignment := haMiddle;
 // TextNode.VerticalAlignment := vaMiddle;
  Add(TextNode);
end;



Const UpwardSpeed=10;

procedure TFloatingTextNode.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
var
  AngleRad: Single;
  FadeFactor: Single;
  HalfLife: Single;
  Drift: TVector3;
begin
  inherited;
  HalfLife := Lifetime / 2;

 // Compute drift direction
  AngleRad := DegToRad(UnitFacing * 60 + 30);
  Drift := Vector3(-Sin(AngleRad), Cos(AngleRad), 0); // Z = 0 to keep depth stable


  Drift := Drift.Normalize * Speed;

  Translation := Translation + Vector3(Drift.X , Drift.Y , 0) * SecondsPassed;


  TimeAlive := TimeAlive + SecondsPassed;

  if TimeAlive < HalfLife then
    FadeFactor := 1.0  // fully visible, no fade
  else
    FadeFactor := 1.0 - ((TimeAlive - HalfLife) / HalfLife); // fade from 1 to 0 during second half

  // Clamp to [0..1]
  if FadeFactor < 0 then
    FadeFactor := 0;


  TextNode.Color := Vector4( Color.X, Color.Y, Color.Z, FadeFactor);

  if TimeAlive >= Lifetime then
    RemoveMe:=rtRemoveAndFree;
end;


{ TUnitsOnMap ---------------------------------------------------------------- }

// Helper function for calculating distance based on your TCubeCoord and OffsetToCube
function GetDistance(const Tile1, Tile2: TVector2Integer): Single;
var
  Cube1, Cube2: TCubeCoord;
  dx, dy, dz: Integer;
begin
  Cube1 := OffsetToCube(Tile1);
  Cube2 := OffsetToCube(Tile2);

  dx := Abs(Cube1.X - Cube2.X);
  dy := Abs(Cube1.Y - Cube2.Y);
  dz := Abs(Cube1.Z - Cube2.Z);

  Result := (dx + dy + dz) / 2.0; // Use 2.0 to ensure float division
end;


procedure TUnitsOnMap.SortGroupMoveList(var AGroupMoveList: TGroupMoveList);
var
  ListToSort: TGroupMoveList;
  MostRightTargetCube: TCubeCoord;
  MostRightTargetFound: Boolean;
  CurrentTargetCube: TCubeCoord;
  GroupCentralTarget: TVector2Integer;
  i: Integer;
  gm: TGroupMove;
begin
  // Determine which list to use based on the parameter
  if AGroupMoveList = nil then
  begin
    // Use the global/class member list if the parameter is nil
    // Make sure FGlobalGroupMoveList is correctly populated before calling with nil.
    ListToSort := FGroupMoveList;
  end
  else
  begin
    // Otherwise, use the list passed as a parameter
    ListToSort := AGroupMoveList;
  end;

  // --- Validation ---
  if ListToSort = nil then
  begin
    // This should ideally not happen if FGlobalGroupMoveList is properly managed
    // or if AGroupMoveList is always passed as non-nil.
    Exit;
  end;
  if ListToSort.Count = 0 then
  begin
    // Nothing to sort.
    Exit;
  end;

  // 1. Determine the "most right" target tile among all calculated TargTile values
  // Initialize MostRightTargetCube with the first unit's target
  MostRightTargetCube := OffsetToCube(ListToSort.Items[0].TargTile);
  MostRightTargetFound := True; // List is not empty, so a target is found

  for i := 1 to ListToSort.Count - 1 do // Start from 1 as 0 is used for initialization
  begin
    CurrentTargetCube := OffsetToCube(ListToSort.Items[i].TargTile);

    // Compare by X (Q) for "most right"
    if CurrentTargetCube.X > MostRightTargetCube.X then
    begin
      MostRightTargetCube := CurrentTargetCube;
    end
    // Tie-breaker: If X (Q) values are equal, prefer the one with a higher Z (R) value
    // This typically means "more down-right" for flat-top hexes.
    // Adjust `CurrentTargetCube.Z > MostRightTargetCube.Z` to `<` if you prefer "more up-right"
    else if (CurrentTargetCube.X = MostRightTargetCube.X) then
    begin
      if CurrentTargetCube.Z > MostRightTargetCube.Z then
        MostRightTargetCube := CurrentTargetCube;
    end;
  end;

  // Convert the most right cube coordinate back to offset for the sorting comparison
  GroupCentralTarget := CubeToOffset(MostRightTargetCube);


  // 2. Sort the ListToSort based on distance from OrigUnit.TilePosition to GroupCentralTarget
  ListToSort.Sort(TComparer<TGroupMove>.Construct(
    function(const L, R: TGroupMove): Integer
    var
      DistL, DistR: Single;
    begin
      // Calculate the distance from each unit's *original position*
      // to the *calculated "most right" group target*.
      DistL := GetDistance(L.OrigUnit.TilePosition, GroupCentralTarget);
      DistR := GetDistance(R.OrigUnit.TilePosition, GroupCentralTarget);

      if DistL < DistR then
        Result := -1 // L comes before R (closer to target)
      else if DistL > DistR then
        Result := 1  // R comes before L
      else
        Result := 0; // Distances are equal
    end
  ));
end;

//creates a TGroupMoveList containing the relpos of the target position for each group unit
function TUnitsOnMap.CreateGroupMoveList(SelectedUnit:TUnit):boolean;
Var gm:TGroupMove;
    i:integer;
    SelList:TUnitList;
    GrUnit:tunit;
begin
   result:=false;
   if assigned(GroupMoveList) then
    GroupMoveList.Free;
   if not GroupList.TryGetValue(SelectedGroup,SelList) then exit;

   FGroupMoveList:=TGroupMoveList.Create(True);
   for i := 0 to SelList.Count-1 do
   begin
      GrUnit := SelList.Items[i];
      gm:=TGroupMove.Create;
      GroupMoveList.Add(gm);
      gm.OrigUnit:=GrUnit;
      gm.Delta := CubeDelta(OffsetToCube(GrUnit.TilePosition),OffsetToCube(SelectedUnit.TilePosition));
      if selectedUnit.IsMoving then
       gm.TargTile:=CubeToOffset(CubeAdd(OffsetToCube(selectedUnit.PathTarget),gm.Delta));
   end;
   //todo:Sort this so first goes the unit that is closer to the nearest target of the group
   SortGroupMoveList(FGroupMoveList);
   result:=true;
end;


//sets all units on group to move
procedure TUnitsOnMap.MoveGroup;
var
  i: Integer;
  TileVector:TVector2Integer;
  grm:TGroupMove;
  NewPath:TList;
begin
  if not assigned(GroupMoveList) then exit;


  for i := 0 to GroupMoveList.Count-1 do
  begin
     grm:=GroupMoveList[i];
     TileVector:=grm.TargTile;

      NewPath:=FindAPath(grm.OrigUnit.TilePosition,grm.TargTile);
      if assigned(NewPath) and (NewPath.Count>0) then
      Begin
        if not assigned(grm.OrigUnit.MovingPath) then //the selected unit laready has a moving path
         grm.OrigUnit.MovingPath := NewPath;
      End
      else NewPath.Free;
  end;
end;


Procedure TUnitsOnMap.SetGroupFacing(Gid,UFace:Integer);
var UList:TUnitList;
    i:integer;
begin
  if Gid>0 then
  begin
   if Grouplist.TryGetValue(GID,UList) then
     for i := 0 to UList.Count-1 do
     begin
        UList[i].UnitFacing:=UFace;
     end;
  end;
end;

procedure TUnitsOnMap.setGroupSelect(GroupID:integer;V:Boolean);
var UList:TUnitList;
    i:integer;
begin
  if not Grouplist.TryGetValue(GroupID,UList) then exit;
  for i := 0 to UList.Count-1 do
     UList[i].Selected:=v;
end;

procedure TUnitsOnMap.GroupSelect(GroupID:integer);
begin
  setGroupSelect(GroupID,true);
  FSelectedGroup:=GroupID;
end;

procedure TUnitsOnMap.GroupUnSelect(GroupID:integer);
begin
  setGroupSelect(GroupID,false);
  FSelectedGroup:=-1;
end;

procedure TUnitsOnMap.AddSelectedUnitsToGroup(GroupId: Integer);
Var Ulist:TUnitList;
    i:integer;
    MinSpeed:Integer;
begin
  MinSpeed := 999;
  if not Grouplist.ContainsKey(Groupid) then
   UList:=NewGroup(GroupID)
  else
   GroupList.TryGetValue(GroupID,UList);
  for i := 0 to UnitsCount-1 do
   if Units[i].Selected then
   begin
     if minSpeed>Units[i].speed then
        minSpeed:=Units[i].speed;
     UList.Add(Units[i]);
     Units[i].GroupID:=GroupId;
   end;
   for i := 0 to UList.Count-1 do
      UList[i].SpeedGroup:=MinSpeed;
end;

function TUnitsOnMap.IsTilePassable(X, Y: Integer): Boolean;
begin
  Result:= not IsTilePassable(Vector2Integer(X,Y));
end;


//returns the tilespeed rename this
function TUnitsOnMap.getUnitSpeed(X, Y: Integer): Integer;
var
  TilePosition : TVector2Integer;
  Tileset: TCastleTiledMapData.TTileset;
  Frame: Integer;
  HorizontalFlip, VerticalFlip, DiagonalFlip: Boolean;
  FrameSpeed:integer;
begin
   TilePosition := Vector2Integer(X,Y);
  // if TileIsBlocked(TilePosition,false) then
  //  exit(0);

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


constructor TUnitsOnMap.Create(const AOwner: TComponent;
  const AMap: TCastleTiledMap);
var i,j : integer;
begin
  inherited Create(AOwner);
  FMap := AMap;
  SetLength(FItems, Map.Data.Width, Map.Data.Height);
  for i := 0 to Map.Data.Width-1 do
    for j := 0 to Map.Data.Height-1 do
       FItems[i,j]:= TUnitlist.Create(False);
  FUnits := TComponentList.Create(false);
  FGroupList:=TGroupList.Create();
  UnitID:=0;
end;

procedure TUnitsOnMap.ClearSelectedUntis;
var i:integer;
begin
  for i:=0 to UnitsCount-1 do
    Units[i].Selected:=false;
end;

procedure TUnitsOnMap.DeleteGroup(GroupID: Integer);
Var UList:TUnitList;
    i:integer;
begin
  if GroupList.ContainsKey(GroupID) then
  begin
    //GroupUnSelect(GroupID);
    if Grouplist.TryGetValue(GroupID,UList) then
      for i :=UList.Count-1 downto 0 do
       UList[i].GroupID:=-1;
    GroupList.Remove(GroupID);//free UList automatically
  end;
end;

destructor TUnitsOnMap.Destroy;
begin
  FreeAndNil(FUnits);
  inherited;
end;

function TUnitsOnMap.FindAPath(StartTile, TargTile: TVector2Integer): TList;
var pathf : TPathfinder;
begin
       pathf := TPathfinder.Create(Vector2TTile(StartTile),Vector2TTile(TargTile));
       try
         pathf.OnIsTilePassable := IsTilePassable;
         pathf.OnGetSpeed := GetUnitSpeed;
       finally
          result := pathf.FindPath;  //list of ttile Records
          pathf.Free;
       end;
end;


function TUnitsOnMap.IsValidPosition(const TilePosition: TVector2Integer):boolean;
begin
  Result:=(TilePosition.X>-1) and (TilePosition.Y>-1) and
       (TilePosition.X<Map.Map.Width) and (TilePosition.Y<Map.Map.Height);
end;


function TUnitsOnMap.GetNextId: integer;
begin
  inc(UnitID);
  result:=UnitID;
end;

function TUnitsOnMap.GetUnitOnMap(const TilePosition: TVector2Integer): TUnit;
begin
  if IsValidPosition(TilePosition) and (FItems[TilePosition.X, TilePosition.Y].Count>0) then
    Result := FItems[TilePosition.X, TilePosition.Y].Items[0] //return by default the first object
  else
   Result := nil;
end;

function TUnitsOnMap.UnitsCount: Integer;
begin
  Result := FUnits.Count;
end;

function TUnitsOnMap.GetUnits(const Index: Integer): TUnit;
begin
  Result := FUnits[Index] as TUnit;
end;

function TUnitsOnMap.UnitsOnTile(
  const TilePosition: TVector2Integer): TUnitList;
begin
  try
   Result := FItems[TilePosition.X, TilePosition.Y];
  except
     Result:=TUnitList.Create;
  end;
end;

procedure TUnitsOnMap.SetUnitsCount(const AValue: Integer);
begin
  if FUnitsCount = AValue then Exit;
  FUnitsCount := AValue;
end;

function TUnitsOnMap.TileOccupied(tile: TVector2Integer): boolean;
var i:integer;
begin
  result := false;
  for i := 0 to UnitsCount-1 do
  begin
   result := result or (Units[i].TileOccupied(tile) and not Units[i].IsMoving);
   if result then break;
  end;
end;

//returns true if tile is NOT Passable
function TUnitsOnMap.TileIsBlocked(const TilePosition: TVector2Integer;IncUnits:boolean=True):boolean;
var
  Tileset: TCastleTiledMapData.TTileset;
  Frame: Integer;
  HorizontalFlip, VerticalFlip, DiagonalFlip: Boolean;
begin
  if (TilePosition.X<0) Or (TilePosition.X>=map.Data.Width)  or
    (TilePosition.Y<0) Or (TilePosition.Y>=map.Data.Height) then
     exit(true);

  Result := Map.Data.TileRenderData(TilePosition,
    Map.Data.Layers[0],
    Tileset, Frame, HorizontalFlip, VerticalFlip, DiagonalFlip) and
    {inpassable is 6}
        (Frame in [5,6,7,10,11]);

   if IncUnits then
     Result := Result or TileOccupied(TilePosition);//no pass over other units
    { Water is on 1, 5, 9 tiles (counting from 0) in data/maps/tileset-terrain.png . }
//    ((Frame mod 4) = 1);
end;

//for event Only
function TUnitsOnMap.IsTilePassable(const TilePosition: TVector2Integer): Boolean;
begin
  Result:=TileIsBlocked(TilePosition);
end;

function TUnitsOnMap.NewGroup(GID:Integer): TUnitList;
begin
  Result:=TunitList.Create(false);
  GroupList.Add(GID,Result);
end;

procedure TUnitsOnMap.setSelectedGroup(const Value: integer);
begin
  if FSelectedGroup>-1 then
   GroupUnSelect(FSelectedGroup);
  FSelectedGroup := Value;
  GroupSelect(FSelectedGroup);
end;

{ TUnit ----------------------------------------------------------------------- }

function TUnit.GetPathTarget: TVector2Integer;
begin
  if assigned(MovingPath) and (MovingPath.Count>0) then
   Result:=TVector2Integer(MovingPath.Items[0]^)
  else
   Result:=TargetTile;
end;

function TUnit.GetSpeed: Integer;
begin
  if IsInGroup then
   result := FSpeedGroup
  else
   result := FSpeed
end;

function TUnit.GroupUnitList:TUnitlist;
begin
 if not UnitsOnMap.FGroupList.TryGetValue(FGroupID,Result) then
   Result:=nil;
end;

procedure TUnit.ClearGroup;
begin
  //removeunit from group
  if FGroupID>-1 then
    GroupUnitList.Remove(Self);
  FGroupID:=-1;
end;

constructor TUnit.Create(AOwner: TComponent);
begin
  inherited;
  if TransformTemplate = nil then
    TransformTemplate := TCastleComponentFactory.Create('castle-data:/unit.castle-transform');
  Transform := TransformTemplate.TransformLoad(Self);

  Background := FindRequiredComponent('Background') as TCastleImageTransform;
  ImageIcon := FindRequiredComponent('ImageIcon') as TCastleImageTransform;
  TextAttack := FindRequiredComponent('TextAttack') as TCastleText;
  TextLife := FindRequiredComponent('TextLife') as TCastleText;
  TextMovement := FindRequiredComponent('TextMovement') as TCastleText;
  SelBox := FindRequiredComponent('SelBox') as TCastleBox;
  TextBattle := FindRequiredComponent('TextBattle') as TCastleText;

  TextBattle.Visible:=false;

  FGroupID:=-1;
  UnitFacing:=0;

end;

procedure TUnit.Initialize(const AUnitsOnMap: TUnitsOnMap;
  const AKind: TUnitKind;
  const AnAttack, AnDefense, AUnits: Integer);
const
  UnitIconUrls: array [TUnitKind] of string  =
  ( 'castle-data:/units/alien1.png',
    'castle-data:/units/alien2.png',
    'castle-data:/units/human1.png',
    'castle-data:/units/human2.png',
    'castle-data:/units/human3.png'
  );
begin
  FKind := AKind;
  FAttack := AnAttack;
  fDefense := AnDefense;
  FUnits := AUnits;

  // adjust components in Transform
  ImageIcon.URL := UnitIconUrls[AKind];
  // change Background.Color (RGB, leave alpha as it was)
  if Human then
    Background.Color := ColorOpacity(HexToColor('FFFFFF'), Background.Color.W)
  else
    Background.Color := ColorOpacity(HexToColor('FFFF00'), Background.Color.W);
  TextAttack.Caption := IntToStr(Attack);
  TextLife.Caption := IntToStr(Units);
  TextMovement.Caption := IntToStr(Defense);

  UnitsOnMap := AUnitsOnMap;
  UnitsOnMap.FUnits.Add(Self);

  PlaceOnMap;
end;

procedure TUnit.Initialize(const AUnitsOnMap: TUnitsOnMap; const AKind: TUnitKind);
var
  Heavy: Boolean;
  AnAttack, AUnits, AnDefense: Integer;
begin
  Heavy := AKind in [ukAlienHeavy, ukHoplites];
  AnAttack := IfThen(Heavy, 7, 3);
  AnDefense := IfThen(Heavy, 10, 2);
  AUnits := IfThen(Heavy, 200, 500);
  Speed:= IfThen(Heavy, 40, 60); //pixels per second 256 pixels to cross a hex

  Initialize(AUnitsOnMap, AKind, AnAttack, AnDefense, AUnits);
  Fid:= AUnitsOnMap.GetNextId;
end;

function TUnit.IsInGroup: Boolean;
begin
   Result:= GroupUnitList<>nil;
end;

destructor TUnit.Destroy;
var  SelList:TUnitList;
begin
  if UnitsOnMap <> nil then
  begin
    UnitsOnMap.FUnits.Remove(Self);
    if GroupID>0 then //remove us from the grouplist too
    begin
      if UnitsOnMap.GroupList.TryGetValue(GroupID,SelList) then
        SelList.Remove(Self);
    end;
    RemoveFromMap;
    UnitsOnMap := nil;
  end;
  inherited;
end;

procedure TUnit.PlaceOnMap;
var
  R: TFloatRectangle;
  Scale: Single;
const
  { Must correspond to the unit.castle-transform original size (see in editor at design-time). }
  UnitDesignedSize = 256.0;
  { Additional unit scale to look better. }
  UnitScale = 0.8;
  { Additional unit scale to look better on Isometric maps. }
  UnitScaleIsometric = 0.75;
begin
  if UnitsOnMap <> nil then
  begin
    UnitsOnMap.FItems[TilePosition.X, TilePosition.Y].Add(Self);
    FTargetTile := TilePosition; //not moving

    R := UnitsOnMap.Map.TileRectangle(TilePosition);
    Transform.Translation := Vector3(R.Center, ZUnit);
    { The unit.castle-transform is designed for tile size = 256.
      Adjust it to match the actual tile size. }
    Scale := UnitScale * R.Height / UnitDesignedSize;
    Transform.Scale := Vector3(Scale, Scale, 1.0);

    Transform.Exists := true;
  end;
end;

procedure TUnit.RemoveFromMap;
begin
  if (UnitsOnMap <> nil) and
     UnitsOnMap.FItems[TilePosition.X, TilePosition.Y].Contains(Self) then
    UnitsOnMap.FItems[TilePosition.X, TilePosition.Y].Remove(Self);

  Transform.Exists := false;
end;

Const BattleEverySecs=2;

function TUnit.getAttackPower:integer;
begin
   result := Attack * Units; //Todo: Add Seasoning
end;

function TUnit.getDefensePower:integer;
begin
   result := Defense * Units;
end;


//if we face the enemy then full attack & defense
//if we dont then
//   if face difference is 1 use a small percentage for att and def
//   if face difference is opposite then attack  and def is minimum
function Tunit.getFacingRatio:Single;
var
  FacingDiff: Integer;
begin
  FacingDiff := TPathfinder.HexFacingDifference(
    UnitFacing,
    Vector2TTile(TilePosition),
    Vector2TTile(BattleWith.TilePosition)
  ) + 1 ; //difference is 0-3 we trabslate to 1-4
  if FacingDiff<=0 then FacingDiff:=1;
  result := 1 /  FacingDiff;
end;

procedure TUnit.DoBattle;
var attpwr,defpwr:Single;
begin
  if SecondsPassed>BattleEverySecs then //battle every x secs
  begin
     SecondsPassed := SecondsPassed - BattleEverySecs;
     //max casualties 20 every 2 secs
     attpwr := AttackPower * FacingRatio;
     defpwr := BattleWith.DefensePower * BattleWith.FacingRatio;
     if defpwr=0 then defpwr:=0.01;
     BattleWith.Casualties := BattleWith.Casualties + Round(20 * (attpwr/defpwr));
  end;
end;

procedure TUnit.checkBattleNearTile;
Var TileArray:TTilearray;
    i:Integer;
    un:TUnit;
begin
   TileArray := TPathFinder.GetHexTilesAtDistance(Vector2TTile(TilePosition),1);
   TPathFinder.SortTilesByFacingPriority(TileArray,UnitFacing,Vector2TTile(TilePosition)); //sort by facing first
   for i := 0 to Length(TileArray)-1 do
   Begin
     un := UnitsOnmap.Items[TTile2Vector(TileArray[i])];
     if (un<>nil) and (un.Human<>Human) then
     begin
        //face the enemy unit
        UnitFacing :=  TPathFinder.GetHexDirection(Vector2TTile(TilePosition),Vector2TTile(un.TilePosition));
        BattleWith:=un;  //attack
        exit;
     end;
   End;
end;

procedure TUnit.CreateCasualtiesAnimation;
var
  FloatingText: TFloatingTextNode;
  s:string;
begin
  s:=inttostr(Casualties);
  FloatingText := TFloatingTextNode.Create(UnitsOnMap.map, s, HexToColor('30012a'), 2.5, 85.0,UnitFacing);
  FloatingText.Translation := Transform.Translation + Vector3(0, 0, 40); // slightly above the unit
  unitsonmap.map.add(FloatingText);
end;

//returns true if unit is destroyed
Function TUnit.EndBattle:boolean;
begin
   if casualties=0 then exit;
   
   FUnits := FUnits - Casualties;
   CreateCasualtiesAnimation;
   Casualties := 0;
   Result := FUnits <= 0 ;
   if not Result then
      TextLife.Caption := IntToStr(FUnits)
   else
     free;
end;

function TUnit.OnBattle:boolean;
begin
  result := FBattleWith<>nil;
end;

procedure TUnit.SetBattleWith(const Value: TUnit);
begin
  if FBattleWith<>Value then
  begin
    if assigned(Value) then
     Value.FreeNotification(Self);
    if assigned(FBattleWith) then
     FBattleWith.RemoveFreeNotification(Self);
    FBattleWith := Value;
    SecondsPassed:=0;
  end;
  //set our unit as battled to the enemy unit if it is not fighting yet
  if assigned(FBattleWith) and (FBattleWith.BattleWith=nil) then
    FBattleWith.BattleWith:=Self;
  TextBattle.Visible:=Assigned(FBattleWith);
end;

procedure TUnit.SetCasualties(const Value: Integer);
begin
  FCasualties := Value;
end;

procedure TUnit.setGroupID(const Value: Integer);
begin
  //if we belong to another group we remove ourselfs
  ClearGroup;
  FGroupID:=Value;
end;

procedure TUnit.SetTilePosition(const Value: TVector2Integer);
begin
  RemoveFromMap;
  FTilePosition := Value;
  PlaceOnMap;
end;

//Is Tile Occupied by us
function TUnit.TileOccupied(Tile: TVector2Integer): boolean;
begin
  result := TVector2Integer.Equals(TilePosition,Tile) or TVector2Integer.Equals(FTargetTile,Tile);
end;

function TUnit.ToString: String;
begin
  Result := Format('%d %s (Attack:%d, Defense:%d, Units:%d)', [
    Id,
    SEnding(GetEnumName(TypeInfo(TUnitKind), Ord(Kind)), 3),
    Attack,
    Defense,
    Units
  ]);
end;


function TUnit.CanMove(const NewTilePosition: TVector2Integer): Boolean;
begin
  Result :=
    (UnitsOnMap <> nil)  and
    not UnitsOnMap.TileIsBlocked(NewTilePosition);
end;

procedure TUnit.Notification(AComponent: TComponent; Operation: TOperation);
var EnemyPosition:TVector2Integer;
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FUnitsOnMap) then
    { set to nil by SetUnitsOnMap to clean nicely }
    UnitsOnMap := nil;
  if (Operation = opRemove) and (AComponent = FBattleWith) then
  Begin
   EnemyPosition:=BattleWith.TilePosition;
   BattleWith:=nil;  //Our Enemy is gone
   checkBattleNearTile; //Find Another
   if not OnBattle and HasMovement then
    PathReached  //continue moving
   else
   if not OnBattle then
      TargetTile:=EnemyPosition;
  End;
end;

procedure TUnit.SetUnits(const Value: Integer);
begin
  FUnits := Value;
end;

procedure TUnit.SetUnitsOnMap(const Value: TUnitsOnMap);
begin
  if FUnitsOnMap <> Value then
  begin
    if FUnitsOnMap <> nil then
      FUnitsOnMap.RemoveFreeNotification(Self);
    FUnitsOnMap := Value;
    if FUnitsOnMap <> nil then
      FUnitsOnMap.FreeNotification(Self);
  end;
end;


procedure TUnit.SetHuman(const Value: Boolean);
begin
  FHuman := Value;
end;

function TUnit.getFacingToTarget:Integer;
Begin
   Result := TPathFinder.GetHexDirection(Tile(TilePosition.X,TilePosition.Y),Tile(TargetTile.X,TargetTile.Y));
End;

function TUnit.getGroupID: Integer;
begin
   Result:=FGroupID;
end;


//Check if the tile we are is valid meaning no other unit occupied it
procedure TUnit.checkValidTile;
var un: TUnit;
    Newpos:TVector2Integer;

    function findposnear(Pos:TVector2Integer;dist:integer):boolean;
    Var AdjTiles: TTileArray;
        i:Integer;
    begin
      if dist>=3 then exit(False); //up to distance 3

      AdjTiles:=TPathfinder.GetHexAdjTiles(Tile(Pos.X,pos.Y));
      for i := 0 to High(AdjTiles) do
      begin
        if not FUnitsOnMap.TileIsBlocked(Vector2Integer(AdjTiles[i].X,AdjTiles[i].Y)) then
        begin
          NewPos:=Vector2Integer(AdjTiles[i].X,AdjTiles[i].Y);
          Exit(True);
        end;
      end;
      //here if no empty
      for i := 0 to High(AdjTiles) do
      begin
        Result:= findposnear(Vector2Integer(AdjTiles[i].X,AdjTiles[i].Y),dist+1);
        If Result then
         break;
      end;
    end;

    procedure MoveToNewPos;
    Begin
       if not TVector2Integer.Equals(TilePosition,Newpos) and not assigned(MovingPath) then
         MovingPath:=UnitsOnMap.FindAPath(TilePosition,NewPos)
       else
       Begin
         MovingPath:=Nil;
         TargetTile:=TilePosition;
       End;
    End;

var mving:boolean;
begin
   Newpos:=TVector2Integer.Zero;
   if FUnitsonMap.UnitsOnTile(tileposition).Count=1 then exit;

   //check if the other unit is moving
   mving:=true;
   for un in FUnitsonMap.UnitsOnTile(tileposition) do
   begin
      if un<>self then
      begin
        if un.MovingPath=nil then
        begin
         mving:=false;
         break;
        end;
      end;
   end;
   if mving then exit;  //all unit are moving so exit

   if findposnear(TilePosition,1)  then
      MoveToNewPos
   else
      OutputDebugString('Can not find empty position!!!');
end;

function TUnit.HasMovement:boolean;
begin
  result := assigned(MovingPath);
end;

function TUnit.IsMoving:boolean;//if onbattle no movement
begin
 result := not OnBattle and (not TVector2Integer.equals(TargetTile,TilePosition) or assigned(MovingPath));
end;

function TUnit.IsOtherUnitHere:Boolean;
var ulist:TUnitList;
begin
   ulist:=UnitsOnMap.UnitsOnTile(TilePosition);
   result:=assigned(ulist) and (ulist.Count>1);
end;

//Current waypoint reached
procedure TUnit.PathReached;
var tile:TTile;
    UnOnTarg:tunit;
    tilelist:tList;
    unfac:integer;

    Procedure DoEndMovement;
    begin
       TargetTile:=TilePosition;
       checkValidTile;
       checkBattleNearTile;
       ClearMovingPath;
    end;

begin
   if Assigned(MovingPath) and (MovingPath.Count>0) then //next tile on path
   begin
    try
     tile:=PathfinderUnit.TTile(MovingPath.Items[MovingPath.Count-1]^);
    except
       MovingPath:=nil;
    end;
     UnOnTarg:=UnitsOnMap.Items[Vector2Integer(tile.X,tile.Y)];
     //check if other unit not ours occupy this tile
     if (UnOnTarg<>nil) and (UnOnTarg.Human<>Human) then
     begin
       if not IsOtherUnitHere then //do Battle
       begin
         BattleWith := UnOnTarg;
         FTargetTile := TilePosition; //no Movement
       end
       else
        checkValidTile; //move to a new tile?
     end
     else //no unit on target tile
     begin
       if CanMove(TTile2Vector(tile)) then
       begin
         TargetTile:= Vector2Integer(tile.X,tile.Y);
         MovingPath.Remove(MovingPath.Items[MovingPath.Count-1]);
       end
       else
       begin
          //no move someone occupies or will occupy the tile
          //find another path or stop and wait if in group
          if GroupID<>-1 then //on group
            exit;
          tile:=Vector2TTile(PathTarget);//final target tile
          ClearMovingPath;
          //save unitfacingsave
          unfac:=FUnitFacingSave;
          MovingPath:=Unitsonmap.FindAPath(TilePosition,TTile2Vector(tile)); //MovingPath autosaves facing
          FUnitFacingSave:=unfac;
          if assigned(MovingPath) then  //new path
          Begin
            tile:=PathfinderUnit.TTile(MovingPath.Items[MovingPath.Count-1]^);//next tile
            MovingPath.Remove(MovingPath.Items[MovingPath.Count-1]);
            TargetTile:= Vector2Integer(tile.X,tile.Y);
          End
          else //no path to the target stop
          begin
            DoEndMovement;
          end;
       end;
     end;
   end
   else //no moving path
   begin
    DoEndMovement;
   end;

end;

procedure TUnit.ClearMovingPath;
begin
 try
 if assigned(fMovingPath) then
   freeandnil(fMovingPath);
 except
   fMovingPath:=nil;
 end;

 fMovingPath:=nil;
end;

procedure TUnit.SetMovingPath(const Value: TList);
var tile:TTile;
begin
  FMovingPath := Value;
  if assigned(Value) then
  begin
   if MovingPath.Count=0 then
      ClearMovingPath
   else
   begin
    tile:= ttile(MovingPath.Items[MovingPath.Count-1]^);
    MovingPath.Remove(MovingPath.Items[MovingPath.Count-1]);
    FUnitFacingSave := UnitFacing; //save starting facing
    if MovingPath.Count=0 then
    begin
      ClearMovingPath;
      TargetTile:=TTile2Vector(tile);
    end
     else TargetTile:=TTile2Vector(ttile(MovingPath.Items[MovingPath.Count-1]^));
    //PathReached;
   end;
  end;
end;

procedure TUnit.SetRange(const Value: Integer);
begin
  FRange := Value;
end;

procedure TUnit.SetSecondsPassed(const Value: Single);
begin
  FSecondsPassed := Value;
end;

procedure TUnit.setSelected(const Value: Boolean);
begin
  FSelected := Value;
  try
    SelBox.Exists:=Value;
  except
     SelBox:=nil;
  end;
end;

procedure TUnit.SetSpeed(const Value: Integer);
begin
  FSpeed := Value;
end;

procedure TUnit.SetSpeedGroup(const Value: Integer);
begin
  FSpeedGroup := Value;
end;

procedure TUnit.SetTargetTile(const Value: TVector2Integer);
begin
  FTargetTile := Value;
  if not TVector2Integer.Equals(FTargetTile,TilePosition) then
   UnitFacing := getFacingToTarget
  else UnitFacing := FUnitFacingSave;//restore original facing
end;

procedure TUnit.SetUnitFacing(const Value: Integer);
begin
  if Value > 5 then
   FUnitFacing :=0
  else
   FUnitFacing := Value;

  case FUnitFacing of
   0:begin
       FaceRotation:=Vector4(0, 0, 1, DegToRad(30));    //NW
     end;
   1:begin
       FaceRotation:=Vector4(0, 0, 1, DegToRad(90));   //W
     end;
   2:begin
       FaceRotation:=Vector4(0, 0, 1, DegToRad(150));  //SW
     end;
   3:begin
       FaceRotation:=Vector4(0, 0, 1, DegToRad(210));  //SE
     end;
   4:begin
       FaceRotation:=Vector4(0, 0, 1, DegToRad(270));  //E
     end;
   5:begin
       FaceRotation:=Vector4(0, 0, 1, DegToRad(330));  //NE
     end;
  end;

 // ImageIcon.Rotation := FaceRotation;
 // Background.Rotation := FaceRotation;
 Transform.Rotation := FaceRotation;

end;

initialization


finalization
  FreeAndNil(TUnit.TransformTemplate);


end.
