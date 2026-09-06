"""Remove only the back-wall oak railing from the existing scene."""
import bpy, os, json, shutil, runpy
from mathutils import Vector

ROOT=os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
path=os.path.join(ROOT,'Manor_Hall_Architecture.blend')
backup=os.path.join(ROOT,'backups','Manor_Hall_before_rear_railing_removal.blend')
os.makedirs(os.path.dirname(backup),exist_ok=True)
if not os.path.exists(backup): shutil.copy2(path,backup)
bpy.ops.wm.open_mainfile(filepath=path)

def rear_railing(ob):
    name=ob.name
    if name.startswith(('Intermediate landing rear newel','Landing back handrail','Landing back spindle')):
        return True
    branch_rail=name.startswith(('West branch','East branch')) and any(
        term in name for term in ('turned oak spindle','continuous handrail','lower newel','upper newel'))
    upper_rail=name.startswith(('Upper landing oak handrail','Upper landing oak spindle'))
    if not (branch_rail or upper_rail): return False
    center=sum((ob.matrix_world @ Vector(corner) for corner in ob.bound_box),Vector())/8
    return center.y > 8.3

removed=[ob.name for ob in bpy.data.objects if rear_railing(ob)]
for name in removed: bpy.data.objects.remove(bpy.data.objects[name],do_unlink=True)
print('REMOVED_REAR_RAILING_PARTS',len(removed),flush=True)
bpy.context.scene['Rear wall railing']='Removed from both stair branches, their rear landing returns, and the central landing.'
bpy.ops.wm.save_as_mainfile(filepath=path)
with open(os.path.join(ROOT,'rear_railing_revision.json'),'w') as f: json.dump({'removed_objects':removed},f,indent=2)
manifest_path=os.path.join(ROOT,'scene_manifest.json')
with open(manifest_path) as f: manifest=json.load(f)
manifest['objects']=len(bpy.data.objects)
with open(manifest_path,'w') as f: json.dump(manifest,f,indent=2)
runpy.run_path(os.path.join(ROOT,'scripts','finalize_manor.py'),run_name='__main__')
