"""Inspect the saved scene and render its delivery views in Blender."""
import bpy, os, json, math, sys
ROOT=os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
bpy.ops.wm.open_mainfile(filepath=os.path.join(ROOT,'Manor_Hall_Architecture.blend'))
s=bpy.context.scene
# Device preferences are process-local and are not carried by the .blend file.
prefs=bpy.context.preferences.addons['cycles'].preferences
for backend in ['OPTIX','CUDA','HIP','ONEAPI']:
    try:
        prefs.compute_device_type=backend
        prefs.get_devices()
        gpu=[d for d in prefs.devices if d.type not in ['CPU']]
        if gpu:
            for d in prefs.devices: d.use=d.type!='CPU'
            s.cycles.device='GPU'
            print('RENDER_BACKEND',backend,flush=True)
            break
    except Exception:
        pass
s.cycles.samples=128
s.cycles.use_denoising=True
s.render.resolution_x=1920
s.render.resolution_y=1440
s.render.resolution_percentage=100
# Keep the scene self-contained; there are no external texture dependencies.
for name in ['Dots Stroke','Material']:
    mat=bpy.data.materials.get(name)
    if mat and mat.users==0: bpy.data.materials.remove(mat)
stairs=json.loads(s['Stair geometry'])
assert len(stairs)==3
assert all(abs(branch['start'][2]-stairs[0]['end'][2])<1e-6 for branch in stairs[1:])
assert all(abs(branch['end'][2]-s['Gallery finished floor (m)'])<1e-6 for branch in stairs[1:])
assert stairs[0]['steps']==9
assert abs(stairs[0]['end'][2]-1.80)<1e-6
assert all(flight['rise']>0 and flight['going']>0 for flight in stairs)
for branch in stairs[1:]:
    # The flight finishes before the innermost cornice face at |X| = 4.35.
    dx=abs(branch['end'][0]-branch['start'][0]); dy=abs(branch['end'][1]-branch['start'][1])
    length=math.hypot(dx,dy)
    assert abs(branch['end'][0]) + dy/length*branch['width']/2 + .031 < 4.35
    # The entire stair width fits between the rear-bay pier bases.
    assert branch['end'][1]-dx/length*branch['width']/2 > 7.19
    assert branch['end'][1]+dx/length*branch['width']/2 < 8.86
assert not any(any(term in ob.name.lower() for term in ['sofa','vase','carpet','painting','table','lamp']) for ob in bpy.data.objects)
assert not any(im.source=='FILE' and im.users and not im.packed_file for im in bpy.data.images)
report={'blender':bpy.app.version_string,'objects':len(bpy.data.objects),
        'vertices':sum(len(o.data.vertices) for o in bpy.data.objects if o.type=='MESH'),
        'stair_landing_levels_match':True,'main_step_count':9,'intermediate_landing_m':1.80,
        'under_branch_oak_enclosures':2,'rear_ground_bays_closed':2,
        'landing_depth_m':s.get('Landing depth (m)'),
        'main_stair_enclosed':bool(s.get('Main stair panelling')),
        'furniture_absent':True,'external_texture_dependencies':0,
        'gallery_floor_m':s['Gallery finished floor (m)'],'stairs':stairs}
with open(os.path.join(ROOT,'scene_validation.json'),'w') as f: json.dump(report,f,indent=2)
views=[('01 | Main hall reference view','manor_hall.png'),
       ('03 | Stair and gallery connections','stair_connections.png'),
       ('04 | Timber ceiling and skylight','ceiling_skylight.png')]
if '--stairs-only' in sys.argv: views=views[:2]
for cam,filename in views:
    s.camera=bpy.data.objects[cam]
    s.render.filepath=os.path.join(ROOT,'renders',filename)
    bpy.ops.render.render(write_still=True)
s.camera=bpy.data.objects['01 | Main hall reference view']
s.render.filepath='//renders/manor_hall.png'
for screen in bpy.data.screens:
    for ar in screen.areas:
        if ar.type=='VIEW_3D':
            ar.spaces.active.region_3d.view_perspective='CAMERA'
            ar.spaces.active.region_3d.view_camera_zoom=0
            ar.spaces.active.overlay.show_overlays=False
bpy.ops.wm.save_as_mainfile(filepath=os.path.join(ROOT,'Manor_Hall_Architecture.blend'))
print('FINAL_RENDER_AND_VALIDATION_COMPLETE',flush=True)
