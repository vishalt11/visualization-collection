"""Apply the nine-step stair and under-branch panelling revision to the saved scene."""
import bpy, math, random, os, json, ast, shutil
from mathutils import Vector
from math import sin,cos,pi,sqrt

ROOT=os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
path=os.path.join(ROOT,'Manor_Hall_Architecture.blend')
backup=os.path.join(ROOT,'backups','Manor_Hall_before_nine_step_panelling.blend')
os.makedirs(os.path.dirname(backup),exist_ok=True)
if not os.path.exists(backup): shutil.copy2(path,backup)
bpy.ops.wm.open_mainfile(filepath=path)
source=open(os.path.join(ROOT,'scripts','build_manor.py'),encoding='utf-8').read()
needed={'mesh','box','beam','extrusion','lathe','rear_flat_wall','newel','stair','horizontal_slab','paneled_stair_enclosure'}
for node in ast.parse(source).body:
    if isinstance(node,ast.FunctionDef) and node.name in needed:
        exec(compile(ast.Module(body=[node],type_ignores=[]),'<builder helpers>','exec'),globals())
stone=[bpy.data.materials['Limestone | ashlar tone %02d'%i] for i in range(9)]
trim=bpy.data.materials['Limestone | dressed pale edges']
woodX=bpy.data.materials['Oak | grain along X']
woodY=bpy.data.materials['Oak | grain along Y']
woodZ=bpy.data.materials['Oak | upright grain']

COL=bpy.data.collections['03 | Ground-floor stone arcades']
removed=[]
for ob in list(COL.objects):
    if not ob.name.startswith(('Ground arch voussoir','Ground shaped spandrel','Rear stair bay flat limestone wall')): continue
    center=sum((ob.matrix_world @ Vector(v) for v in ob.bound_box),Vector())/8
    if center.y>6.8:
        removed.append(ob.name); bpy.data.objects.remove(ob,do_unlink=True)
for x in [-4.85,4.85]: rear_flat_wall(x)

COL=bpy.data.collections['06 | Oak split staircase - connected landings']
for ob in list(COL.objects): bpy.data.objects.remove(ob,do_unlink=True)
start=source.index('stair_data=[]')
end=source.index("collection('07 |",start)
exec(compile(source[start:end],'<revised staircase>','exec'),globals())
s=bpy.context.scene
s['Stair geometry']=json.dumps(stair_data)
s['Intermediate landing finished floor (m)']=LANDING
s['Main staircase treads']=9
s['Under branch panelling']=json.dumps(enclosure_data)
s['Rear ground-floor archways']='Two rear bays replaced with flat limestone walls behind oak stair enclosures.'
s['Deferred detail']='Yellow starter plinth is deferred; no decorative starter plinth added.'
cam=bpy.data.objects['03 | Stair and gallery connections']
cam.location=(-1.2,-3.0,5.8)
cam.rotation_euler=(Vector((0,5.6,2.55))-cam.location).to_track_quat('-Z','Y').to_euler()
cam.data.lens=25
with open(os.path.join(ROOT,'scene_manifest.json')) as f: manifest=json.load(f)
manifest.update(objects=len(bpy.data.objects),stairs=stair_data,under_branch_panelling=enclosure_data)
with open(os.path.join(ROOT,'scene_manifest.json'),'w') as f: json.dump(manifest,f,indent=2)
bpy.ops.wm.save_as_mainfile(filepath=path)
print('NINE_STEP_PANEL_REVISION_SAVED',len(removed),'rear arch objects replaced',flush=True)
prefs=bpy.context.preferences.addons['cycles'].preferences
prefs.compute_device_type='OPTIX'; prefs.get_devices()
for dev in prefs.devices: dev.use=dev.type!='CPU'
s.cycles.device='GPU'; s.cycles.samples=48
s.render.resolution_x=1280; s.render.resolution_y=960; s.render.resolution_percentage=100
for camname,filename in [('01 | Main hall reference view','manor_hall_preview.png'),('03 | Stair and gallery connections','panelling_preview.png')]:
    s.camera=bpy.data.objects[camname]
    s.render.filepath=os.path.join(ROOT,'renders',filename)
    bpy.ops.render.render(write_still=True)
print('REVISION_PREVIEWS_COMPLETE',flush=True)
