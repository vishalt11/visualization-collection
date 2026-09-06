"""Shorten only the landing and translate the nine-step flight; preserve branches."""
import bpy, os, ast, json, shutil, math
from mathutils import Vector
from math import sin,cos,pi,sqrt
ROOT=os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
path=os.path.join(ROOT,'Manor_Hall_Architecture.blend')
backup=os.path.join(ROOT,'backups','Manor_Hall_before_shortened_landing.blend')
if not os.path.exists(backup): shutil.copy2(path,backup)
bpy.ops.wm.open_mainfile(filepath=path)
COL=bpy.data.collections['06 | Oak split staircase - connected landings']
source=open(os.path.join(ROOT,'scripts','build_manor.py'),encoding='utf-8').read()
for node in ast.parse(source).body:
    if isinstance(node,ast.FunctionDef) and node.name in {'mesh','box','beam','extrusion','main_stair_panelling'}:
        exec(compile(ast.Module(body=[node],type_ignores=[]),'<helpers>','exec'),globals())
woodX=bpy.data.materials['Oak | grain along X']; woodY=bpy.data.materials['Oak | grain along Y']; woodZ=bpy.data.materials['Oak | upright grain']
s=bpy.context.scene
stairs=json.loads(s['Stair geometry']); main=stairs[0]
shift=7.25-main['end'][1]
protected={ob.name:tuple(ob.matrix_world) for ob in COL.objects if ob.name.startswith(('West branch','East branch'))}
for ob in list(COL.objects):
    if ob.name.startswith('Main flight'): ob.location.y+=shift
    elif ob.name.startswith(('Main stair ','Landing enclosed oak support','Landing front connecting')):
        bpy.data.objects.remove(ob,do_unlink=True)
    elif ob.name.startswith(('Central intermediate landing','Landing oak floorboard')):
        ob.scale.y*=1.33/ob.dimensions.y
        ob.location.y=7.845
main['start'][1]+=shift; main['end'][1]+=shift
main_stair_panelling(main['start'][1],main['end'][1],main['start'][2],main['end'][2],9)
for side in [-1,1]:
    beam('Landing front connecting rail',(side*1.585,7.25,2.82),(side*1.72,7.34,2.82),.14,.14,woodX,.024)
bpy.context.view_layer.update()
assert len([o for o in COL.objects if o.name.startswith('Main flight tread')])==9
assert all(tuple(bpy.data.objects[name].matrix_world)==transform for name,transform in protected.items())
s['Stair geometry']=json.dumps(stairs)
s['Landing depth (m)']=1.33
s['Main stair panelling']='Closed oak core with matching panelling on both exposed sides and a closed landing support.'
with open(os.path.join(ROOT,'scene_manifest.json')) as f: manifest=json.load(f)
manifest.update(objects=len(bpy.data.objects),stairs=stairs,landing_depth_m=1.33,main_stair_enclosed=True)
with open(os.path.join(ROOT,'scene_manifest.json'),'w') as f: json.dump(manifest,f,indent=2)
bpy.ops.wm.save_as_mainfile(filepath=path)
print('LANDING_SHORTENED_BRANCHES_PRESERVED',len(protected),'objects; main tread count 9',flush=True)
prefs=bpy.context.preferences.addons['cycles'].preferences
prefs.compute_device_type='OPTIX'; prefs.get_devices()
for d in prefs.devices: d.use=d.type!='CPU'
s.cycles.device='GPU'; s.cycles.samples=48
s.render.resolution_x=1280; s.render.resolution_y=960; s.render.resolution_percentage=100
# A temporary oblique view makes the new stair side panels visible for inspection.
cam=s.camera=bpy.data.objects['03 | Stair and gallery connections']
oldloc=cam.location.copy(); oldrot=cam.rotation_euler.copy(); oldlens=cam.data.lens
cam.location=(-3.5,0.0,4.2); cam.rotation_euler=(Vector((0,6.5,1.7))-cam.location).to_track_quat('-Z','Y').to_euler(); cam.data.lens=28
s.render.filepath=os.path.join(ROOT,'renders','landing_side_preview.png')
bpy.ops.render.render(write_still=True)
cam.location=oldloc; cam.rotation_euler=oldrot; cam.data.lens=oldlens
print('LANDING_PREVIEW_COMPLETE',flush=True)
