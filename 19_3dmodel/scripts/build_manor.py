"""Rebuild an unfurnished Croft-inspired manor hall with editable Blender geometry.
Run: blender --background --factory-startup --python scripts/build_manor.py
"""
import bpy, math, random, os, json, sys
from mathutils import Vector
from math import sin, cos, pi, sqrt

random.seed(81)
ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
bpy.ops.object.select_all(action='SELECT')
bpy.ops.object.delete(use_global=False)
for c in list(bpy.data.collections):
    if c.name != 'Collection': bpy.data.collections.remove(c)
base = bpy.data.collections.get('Collection')
base.name = '00 | Scene setup'
COL = base
def collection(name):
    global COL
    COL = bpy.data.collections.new(name)
    bpy.context.scene.collection.children.link(COL)
    return COL

def material(name, color, rough=.6, metallic=0):
    m=bpy.data.materials.new(name); m.diffuse_color=(*color,1); m.use_nodes=True
    p=m.node_tree.nodes.get('Principled BSDF')
    p.inputs['Base Color'].default_value=(*color,1)
    p.inputs['Roughness'].default_value=rough
    p.inputs['Metallic'].default_value=metallic
    return m

def stone_material(name, low, high):
    m=material(name,high,.78); n=m.node_tree.nodes; l=m.node_tree.links
    p=n.get('Principled BSDF'); coord=n.new('ShaderNodeTexCoord')
    noise=n.new('ShaderNodeTexNoise'); noise.inputs['Scale'].default_value=3.8; noise.inputs['Detail'].default_value=5
    l.new(coord.outputs['Object'],noise.inputs['Vector'])
    ramp=n.new('ShaderNodeValToRGB'); ramp.color_ramp.elements[0].position=.14; ramp.color_ramp.elements[0].color=(*low,1)
    ramp.color_ramp.elements[1].position=.85; ramp.color_ramp.elements[1].color=(*high,1)
    l.new(noise.outputs['Fac'],ramp.inputs[0]); l.new(ramp.outputs[0],p.inputs['Base Color'])
    fine=n.new('ShaderNodeTexNoise'); fine.inputs['Scale'].default_value=92; fine.inputs['Detail'].default_value=2
    l.new(coord.outputs['Object'],fine.inputs['Vector'])
    bump=n.new('ShaderNodeBump'); bump.inputs['Strength'].default_value=.24; bump.inputs['Distance'].default_value=.032
    l.new(fine.outputs['Fac'],bump.inputs['Height']); l.new(bump.outputs[0],p.inputs['Normal'])
    return m

stone=[]
for i in range(9):
    v=.88+i*.026
    stone.append(stone_material('Limestone | ashlar tone %02d'%i, (.26*v,.235*v,.192*v),(.58*v,.535*v,.448*v)))
trim=stone_material('Limestone | dressed pale edges',(.33,.30,.25),(.65,.61,.52))
darkstone=stone_material('Floor | charcoal limestone',(.075,.081,.076),(.15,.16,.146))
grout=material('Recessed lime mortar',(.20,.19,.16),.94)
black=material('Firebox | soot-dark stone',(.045,.042,.035),.98)
lead=material('Window | aged lead cames',(.048,.057,.050),.37,.68)

def wood_material(name, axis, light=False):
    m=material(name,(.18,.074,.024),.39); n=m.node_tree.nodes; l=m.node_tree.links; p=n.get('Principled BSDF')
    tex=n.new('ShaderNodeTexCoord'); scale=n.new('ShaderNodeVectorMath'); scale.operation='MULTIPLY'
    factors=[7,7,7]; factors[axis]=.4; scale.inputs[1].default_value=factors
    l.new(tex.outputs['Object'],scale.inputs[0])
    no=n.new('ShaderNodeTexNoise'); no.inputs['Scale'].default_value=3; no.inputs['Detail'].default_value=3; no.inputs['Roughness'].default_value=.72
    l.new(scale.outputs[0],no.inputs[0]); ramp=n.new('ShaderNodeValToRGB')
    ramp.color_ramp.elements[0].position=.17; ramp.color_ramp.elements[0].color=(.035,.012,.004,1)
    ramp.color_ramp.elements[1].position=.84; ramp.color_ramp.elements[1].color=((.31,.145,.05,1) if light else (.205,.085,.022,1))
    l.new(no.outputs['Fac'],ramp.inputs[0]); l.new(ramp.outputs[0],p.inputs['Base Color'])
    b=n.new('ShaderNodeBump'); b.inputs['Strength'].default_value=.17; b.inputs['Distance'].default_value=.023
    l.new(no.outputs['Fac'],b.inputs['Height']); l.new(b.outputs[0],p.inputs['Normal'])
    return m
woodX=wood_material('Oak | grain along X',0,True)
woodY=wood_material('Oak | grain along Y',1)
woodZ=wood_material('Oak | upright grain',2)

def mesh(name, verts, faces, mat, bevel=0):
    me=bpy.data.meshes.new(name); me.from_pydata(verts,[],faces); me.update()
    ob=bpy.data.objects.new(name,me); COL.objects.link(ob)
    if mat: me.materials.append(mat)
    if bevel:
        mod=ob.modifiers.new('Small worn edges','BEVEL'); mod.width=bevel; mod.segments=2
    return ob

def box(name, loc, size, mat, bevel=.015):
    x,y,z=[a/2 for a in size]
    verts=[(-x,-y,-z),(-x,-y,z),(-x,y,-z),(-x,y,z),(x,-y,-z),(x,-y,z),(x,y,-z),(x,y,z)]
    ob=mesh(name,verts,[(0,4,6,2),(1,3,7,5),(0,1,5,4),(2,6,7,3),(0,2,3,1),(4,5,7,6)],mat,bevel)
    ob.location=loc
    return ob

def beam(name,a,b,width,depth,mat,bevel=.016):
    a,b=Vector(a),Vector(b); ob=box(name,(a+b)/2,(width,depth,(b-a).length),mat,bevel)
    ob.rotation_euler=(b-a).to_track_quat('Z','Y').to_euler(); return ob

def extrusion(name, poly, depth, axis, fixed, mat, bevel=.012):
    # 2D coordinates: horizontal span and height. Extrude along wall depth.
    verts=[]
    for d in [-depth/2,depth/2]:
        for u,z in poly:
            verts.append((fixed+d,u,z) if axis=='X' else (u,fixed+d,z))
    N=len(poly); faces=[tuple(range(N-1,-1,-1)),tuple(range(N,2*N))]
    faces.extend((i,(i+1)%N,(i+1)%N+N,i+N) for i in range(N))
    return mesh(name,verts,faces,mat,bevel)

def lathe(name, x,y,z,h,rad,mat):
    profile=[(0,.68),(.07,.68),(.10,.42),(.19,.28),(.35,.47),(.47,.55),(.60,.38),(.74,.24),(.89,.34),(.93,.64),(1,.64)]
    seg=12; verts=[]
    for zz,r in profile:
        verts.extend((x+rad*r*cos(2*pi*k/seg),y+rad*r*sin(2*pi*k/seg),z+zz*h) for k in range(seg))
    faces=[]
    for i in range(len(profile)-1):
        for k in range(seg): faces.append((i*seg+k,i*seg+(k+1)%seg,(i+1)*seg+(k+1)%seg,(i+1)*seg+k))
    faces.extend([tuple(range(seg-1,-1,-1)),tuple((len(profile)-1)*seg+k for k in range(seg))])
    ob=mesh(name,verts,faces,mat)
    for f in ob.data.polygons: f.use_smooth=True
    return ob

collection('01 | Limestone floor and foundations')
box('Hall foundation',(0,0,-.30),(14.7,20,.6),stone[2],.07)
box('Floor mortar bed',(0,0,-.03),(14,19.6,.08),grout,0)
for ix in range(14):
    for iy in range(20):
        x=-6.5+ix; y=-9.5+iy
        mat=darkstone if (ix in [5,8] and iy in [2,5,8,11,14]) else random.choice(stone)
        box('Floor flag %02d.%02d'%(ix,iy),(x,y,-.005),(.982,.982,.11),mat,.018)
# Perimeter stone band aligns exactly with the room.
for x in [-6.86,6.86]: box('Perimeter border',(x,0,.06),(.22,20,.08),trim)

collection('02 | Exterior limestone walls')
def courses_side(x):
    box('Wall masonry core',(x,0,5.65),(.5,20,11.3),grout,0)
    for r in range(21):
        lo=-10; count=16; step=20/count
        edges=[lo]+[lo+k*step+(step/2 if r%2 else 0) for k in range(1,count)]+[10]
        for a,b in zip(edges[:-1],edges[1:]):
            box('Side ashlar course %02d'%r,(x,(a+b)/2,(r+.5)*.535),(.56,b-a-.018,.519),random.choice(stone),.013)
    for zz,h,w in [(.20,.4,.73),(4.57,.25,.79),(5.01,.14,.69),(10.77,.20,.83),(11.05,.24,.96)]:
        box('Wall continuous cornice',(x,0,zz),(w,20,h),trim,.026)
for x in [-7.15,7.15]: courses_side(x)

# Rear wall built around a real arched window opening; no opaque wall behind glass.
WIN_R=2.15; WIN_BASE=4.20; WIN_SPRING=8.03; WIN_TOP=10.18
for r in range(21):
    z0=r*.535; z1=z0+.519; zc=(z0+z1)/2
    if z1<=WIN_BASE or z0>=WIN_TOP:
        spans=[(-7.45,7.45)]
    else:
        radius=WIN_R if z0<=WIN_SPRING else sqrt(max(0,WIN_R**2-(z0-WIN_SPRING)**2))
        spans=[(-7.45,-radius-.025),(radius+.025,7.45)]
    for a,b in spans:
        count=max(1,round((b-a)/1.15))
        for j in range(count):
            u=a+(b-a)*j/count; v=a+(b-a)*(j+1)/count
            box('Rear wall ashlar',( (u+v)/2,9.78,zc),(v-u-.014,.66,z1-z0),random.choice(stone),.012)
# Curved rear spandrel fills small gaps left by horizontal block courses.
for k in range(32):
    t0=pi*k/32; t1=pi*(k+1)/32
    p0=(WIN_R*cos(t0),WIN_SPRING+WIN_R*sin(t0)); p1=(WIN_R*cos(t1),WIN_SPRING+WIN_R*sin(t1))
    extrusion('Rear window spandrel',[p0,p1,(p1[0],11.23),(p0[0],11.23)],.60,'Y',9.80,random.choice(stone),.008)
for x in [-7,7]: box('Rear corner quoin',(x,9.36,5.6),(.68,.3,11.2),trim,.028)
for z in [.25,10.78,11.08]: box('Rear wall cornice',(0,9.38,z),(14.4,.35,.20),trim,.02)

collection('03 | Ground-floor stone arcades')
PIERS=[-9.6,-5.5,-1.4,2.7,6.8,9.25]
def pier(x,y,z0,height,upper=False):
    width=.62 if upper else .72
    box('Pier square base',(x,y,z0+.12),(.92,.94,.24),trim,.035)
    box('Pier base bevel',(x,y,z0+.31),(.81,.82,.14),trim,.024)
    for k in range(max(1,int((height-.65)/.55))):
        n=max(1,int((height-.65)/.55)); h=(height-.65)/n
        box('Pier shaft block',(x,y,z0+.40+(k+.5)*h),(width,width,h-.012),random.choice(stone),.014)
    box('Pier neck',(x,y,z0+height-.21),(.79,.80,.14),trim,.02)
    box('Pier capital',(x,y,z0+height-.07),(.97,.98,.17),trim,.027)

def arch(x,a,b,spring,rise,top,level):
    c=(a+b)/2; radius=(b-a-.78)/2; outer=radius+.28
    for k in range(15):
        t0=k*pi/15+.007; t1=(k+1)*pi/15-.007
        poly=[(c+radius*cos(t0),spring+rise*sin(t0)),(c+outer*cos(t0),spring+(rise+.28)*sin(t0)),(c+outer*cos(t1),spring+(rise+.28)*sin(t1)),(c+radius*cos(t1),spring+rise*sin(t1))]
        extrusion(level+' arch voussoir %02d'%k,poly,.79,'X',x,trim if k==7 else random.choice(stone),.014)
    # Shape the solid spandrel above the extrados without blocking the opening.
    for k in range(20):
        t0=k*pi/20; t1=(k+1)*pi/20
        u=c+outer*cos(t0); v=c+outer*cos(t1)
        extrusion(level+' shaped spandrel',[(u,spring+(rise+.29)*sin(t0)),(v,spring+(rise+.29)*sin(t1)),(v,top),(u,top)],.62,'X',x,random.choice(stone),.006)
    for u in [a,b]:
        box(level+' over-pier masonry',(x,u,(spring+top)/2),(.64,.74,top-spring),stone[4],.008)

def rear_flat_wall(x):
    # Rear ground-floor bay is closed masonry behind the stair enclosure.
    for row in range(9):
        for col in range(3):
            box('Rear stair bay flat limestone wall',
                (x,6.8+(col+.5)*2.45/3,(row+.5)*4.48/9),
                (.79,2.45/3-.015,4.48/9-.014),stone[(row+col)%len(stone)],.012)

for x in [-4.85,4.85]:
    for y in PIERS: pier(x,y,0,3.0)
    for a,b in zip(PIERS[:-1],PIERS[1:]):
        if a>=6.8: rear_flat_wall(x)
        else: arch(x,a,b,3.0,1.12,4.48,'Ground')
    for z,w,h in [(4.42,.85,.18),(4.59,1.00,.16),(4.73,.90,.13)]:
        box('Gallery layered stringcourse',(x,-.17,z),(w,19.85,h),trim,.028)
    # Dentils tucked below the gallery cornice.
    for j in range(65): box('Cornice dentil',(x-math.copysign(.43,x),-9.55+j*.294,4.40),(.17,.13,.18),trim,.01)

collection('04 | Upper galleries and balustrades')
for side in [-1,1]:
    x=side*4.85
    box('Continuous walkable gallery',(side*6.0,-.18,4.60),(2.35,19.82,.35),stone[3],.025)
    for j in range(39):
        box('Gallery limestone paving',(side*6.,-9.65+j*.50,4.805),(2.28,.482,.07),random.choice(stone),.012)
    for y in PIERS: pier(x,y,4.84,3.04,True)
    for a,b in zip(PIERS[:-1],PIERS[1:]):
        rise=1.38 if b-a>3 else .80
        arch(x,a,b,7.88,rise,10.90,'Upper')
    box('Upper arcade crown',(x,-.16,10.92),(1.00,19.95,.23),trim,.035)
    for a,b in zip(PIERS[:-2],PIERS[1:-1]):
        # Rearmost bay intentionally open: the stair branches enter here.
        lo=a+.46; hi=b-.46
        box('Balustrade bottom rail',(x,(lo+hi)/2,4.96),(.48,hi-lo,.16),trim,.026)
        box('Balustrade handrail',(x,(lo+hi)/2,5.91),(.52,hi-lo,.17),trim,.03)
        count=max(2,int((hi-lo)/.27))
        for j in range(count): lathe('Turned limestone baluster',x,lo+(j+.5)*(hi-lo)/count,5.04,.79,.13,trim)

collection('05 | Limestone fireplace and architectural surrounds')
# Unfurnished architectural fireplace in the right wall, with an empty firebox.
box('Fireplace dark inset',(6.865,.60,1.35),(.04,2.55,2.50),black,0)
box('Fireplace hearth',(6.45,.60,.12),(1.42,3.48,.24),trim,.045)
for y in [-.89,2.09]:
    box('Fireplace jamb',(6.52,y,1.30),(.85,.40,2.44),trim,.025)
    box('Fireplace jamb foot',(6.47,y,.33),(.96,.57,.35),trim,.028)
    box('Fireplace jamb capital',(6.47,y,2.43),(.95,.62,.20),trim,.026)
box('Fireplace lintel',(6.48,.6,2.71),(.95,3.5,.40),trim,.035)
box('Fireplace mantel',(6.39,.6,2.99),(1.12,3.77,.19),trim,.038)
box('Fireplace frieze',(6.70,.6,3.57),(.70,3.20,.98),stone[5],.025)
for y in [-.55,1.75]: box('Fireplace frieze pilaster',(6.28,y,3.57),(.12,.13,.89),trim,.02)
box('Fireplace upper moulding',(6.53,.6,4.15),(.97,3.61,.21),trim,.027)

collection('06 | Oak split staircase - connected landings')
def newel(name, pos):
    x,y,z=pos
    box(name+' base',(x,y,z+.12),(.26,.26,.24),woodZ,.022)
    box(name+' post',(x,y,z+.61),(.17,.17,.89),woodZ,.018)
    box(name+' collar',(x,y,z+1.01),(.24,.24,.10),woodX,.018)
    # Faceted carved finial, part of the stair joinery.
    lathe(name+' finial',x,y,z+1.06,.25,.145,woodZ)

def stair(name,start,end,z0,z1,width,steps,closed_start=True):
    a=Vector((start[0],start[1],0)); b=Vector((end[0],end[1],0)); direction=(b-a).normalized()
    length=(b-a).length; side=Vector((direction.y,-direction.x,0)); run=length/steps; rise=(z1-z0)/steps
    angle=math.atan2(-direction.x,direction.y)
    # Stair underside is a real sloping timber slab.
    aa=a+Vector((0,0,z0-.13)); bb=b+Vector((0,0,z1-.13))
    ob=beam(name+' structural underside',aa,bb,width,.22,woodY,.018)
    # Beam helper's local Y may rotate: explicit longitudinal stringers below tread edges.
    for s in [-1,1]:
        edge=side*s*(width/2-.08)
        beam(name+' side stringer',a+edge+Vector((0,0,z0-.15)),b+edge+Vector((0,0,z1-.15)),.20,.30,woodY)
    for i in range(steps):
        z=z0+(i+1)*rise; p=a+direction*((i+.5)*run)
        ob=box(name+' tread %02d'%(i+1),(p.x,p.y,z-.045),(width+.09,run+.06,.09),woodX,.024); ob.rotation_euler.z=angle
        p=a+direction*(i*run+.018)
        ob=box(name+' riser %02d'%(i+1),(p.x,p.y,z-rise/2-.025),(width,.055,rise),woodX,.012); ob.rotation_euler.z=angle
    for s in [-1,1]:
        edge=side*s*(width/2-.09)
        # The back-wall side of both branches is intentionally unrailed.
        if name != 'Main flight' and edge.y > 0:
            continue
        for i in range(steps):
            t=(i+.5)/steps; p=a+(b-a)*t+edge; z=z0+(i+1)*rise
            lathe(name+' turned oak spindle',p.x,p.y,z,.82,.064,woodZ)
        p=a+edge+Vector((0,0,z0+1.02)); q=b+edge+Vector((0,0,z1+1.02))
        beam(name+' continuous handrail',p,q,.14,.14,woodY,.027)
        newel(name+' lower newel',a+edge+Vector((0,0,z0)))
        newel(name+' upper newel',b+edge+Vector((0,0,z1)))
    return {'name':name,'start':[a.x,a.y,z0],'end':[b.x,b.y,z1],'width':width,'steps':steps,'rise':rise,'going':run}

def horizontal_slab(name,poly,z,thickness,mat):
    vertices=[(x,y,h) for h in [z-thickness,z] for x,y in poly]
    n=len(poly)
    faces=[tuple(range(n-1,-1,-1)),tuple(range(n,2*n))]
    faces.extend((i,(i+1)%n,(i+1)%n+n,i+n) for i in range(n))
    return mesh(name,vertices,faces,mat,.012)

def paneled_stair_enclosure(name,start,end,z0,z1,width):
    a=Vector((start[0],start[1],0)); b=Vector((end[0],end[1],0))
    direction=(b-a).normalized(); length=(b-a).length
    face=Vector((direction.y,-direction.x,0))
    if face.y>0: face=-face
    front=a+face*(width/2-.025)
    def panel(name,poly,depth,offset,mat,bevel=.013):
        verts=[]
        for d in [-depth/2,depth/2]:
            for u,z in poly:
                p=front+direction*u+face*(offset+d); verts.append((p.x,p.y,z))
        n=len(poly); faces=[tuple(range(n-1,-1,-1)),tuple(range(n,2*n))]
        faces.extend((i,(i+1)%n,(i+1)%n+n,i+n) for i in range(n))
        return mesh(name,verts,faces,mat,bevel)
    def top(u): return z0-.14+(z1-z0)*u/length
    # A full-width closed timber volume replaces the void under each branch.
    panel(name+' solid oak enclosure',[(0,.055),(length,.055),(length,top(length)),(0,top(0))],
          width,-width/2,woodZ,.018)
    count=5; spacing=length/count
    for i in range(count):
        u0=i*spacing+.09; u1=(i+1)*spacing-.09
        for row in range(5):
            low=.25+row*.86; h=.70
            t0=min(low+h,top(u0)-.14); t1=min(low+h,top(u1)-.14)
            if min(t0,t1)<low+.20: continue
            outer=[(u0,low),(u1,low),(u1,t1),(u0,t0)]
            panel(name+' fielded panel moulding',outer,.06,.038,woodX,.018)
            inner=[(u0+.055,low+.055),(u1-.055,low+.055),(u1-.055,t1-.06),(u0+.055,t0-.06)]
            panel(name+' recessed oak field',inner,.025,.076,woodZ,.026)
    for i in range(count+1):
        u=i*spacing; lo=max(0,u-.038); hi=min(length,u+.038)
        panel(name+' vertical panel stile',[(lo,.18),(hi,.18),(hi,top(hi)-.04),(lo,top(lo)-.04)],.10,.055,woodZ)
    panel(name+' oak skirting',[(0,.065),(length,.065),(length,.23),(0,.23)],.15,.067,woodX,.025)
    for u,v in [(0,length)]:
        panel(name+' sloped panel crown',[(u,top(u)-.10),(v,top(v)-.10),(v,top(v)+.045),(u,top(u)+.045)],.14,.055,woodX,.022)
    return {'name':name,'length':length,'floor':.055,'top_start':top(0),'top_end':top(length)}

def main_stair_panelling(start_y,end_y,z0,z1,steps):
    # Closed stair-shaped oak core reaches the underside of every tread.
    run=(end_y-start_y)/steps; rise=(z1-z0)/steps
    top=[]
    for i in range(steps):
        z=z0+(i+1)*rise-.09
        top.extend([(start_y+i*run,z),(start_y+(i+1)*run,z)])
    poly=[(start_y,.052),(end_y,.052)]+list(reversed(top))
    extrusion('Main stair enclosed oak core',poly,3.24,'X',0,woodZ,.009)
    # Both exposed side faces receive the same fielded oak detailing as branches.
    def upper(y): return z0+(z1-z0)*(y-start_y)/(end_y-start_y)-.12
    for side in [-1,1]:
        x=side*1.635
        for i in range(5):
            lo=start_y+i*(end_y-start_y)/5+.045
            hi=start_y+(i+1)*(end_y-start_y)/5-.045
            zlo=max(.15,upper(lo)); zhi=max(.15,upper(hi))
            if min(zlo,zhi)<.34: continue
            outline=[(lo,.15),(hi,.15),(hi,zhi),(lo,zlo)]
            extrusion('Main stair side panel moulding',outline,.055,'X',x,woodX,.018)
            inset=[(lo+.045,.205),(hi-.045,.205),(hi-.045,zhi-.06),(lo+.045,zlo-.06)]
            extrusion('Main stair recessed oak field',inset,.023,'X',x+side*.038,woodZ,.024)
        box('Main stair oak skirting',(side*1.635,(start_y+end_y)/2,.095),(.11,end_y-start_y,.08),woodX,.012)
    # Close the landing support and the small returns behind the main flight.
    box('Landing enclosed oak support',(0,7.845,.84),(3.56,1.33,1.57),woodZ,.018)

stair_data=[]
LANDING=1.80
stair_data.append(stair('Main flight',(0,4.55),(0,7.25),.075,LANDING,3.35,9))
# The front edge is pulled back to the branch entries; the rear edge stays fixed.
box('Central intermediate landing',(0,7.845,LANDING-.1675),(3.62,1.33,.28),woodX,.025)
for i in range(10): box('Landing oak floorboard',(-1.62+i*.36,7.845,LANDING-.0275),(.345,1.33,.055),woodY,.009)
main_stair_panelling(4.55,7.25,.075,LANDING,9)
enclosure_data=[]
for s in [-1,1]:
    label=('West' if s<0 else 'East')+' branch'
    start=(s*1.72,7.99); end=(s*4.30,7.99); width=1.48
    stair_data.append(stair(label,start,end,LANDING,4.84,width,12))
    enclosure_data.append(paneled_stair_enclosure(label,start,end,LANDING,4.84,width))
    d=Vector((end[0]-start[0],end[1]-start[1],0)).normalized()
    front=Vector((d.y,-d.x,0))
    if front.y>0: front=-front
    a=Vector((end[0],end[1],0))+front*width/2
    b=Vector((end[0],end[1],0))-front*width/2
    # The deck starts at the top-tread edge, never over earlier steps.
    poly=[(a.x,a.y),(s*6.42,a.y),(s*6.42,b.y),(b.x,b.y)]
    if s<0: poly.reverse()
    horizontal_slab(label+' upper landing deck',poly,4.84,.22,woodX)
    # Short exposed front return joins the stone gallery balustrade.
    beam(label+' upper landing front rail',(a.x,a.y,5.86),(s*4.85,a.y,5.86),.14,.14,woodX,.024)
    for t in [.25,.55,.82]:
        x=a.x+(s*4.85-a.x)*t
        lathe(label+' upper landing front spindle',x,a.y,4.84,.89,.065,woodZ)
    p=Vector((start[0],start[1],0))+front*(width/2-.09)
    q=Vector((s*(3.35/2-.09),7.25,0))
    beam('Landing front connecting rail',(q.x,q.y,LANDING+1.02),(p.x,p.y,LANDING+1.02),.14,.14,woodX,.024)
    if (p-q).length>.30:
        for t in [.24,.50,.76]:
            v=q+(p-q)*t; lathe('Landing front connecting spindle',v.x,v.y,LANDING,.89,.065,woodZ)
# No rear railing or decorative starter plinth in this revision.

collection('07 | Leaded stained glass - landscape window')
for x in [-WIN_R-.14,WIN_R+.14]:
    box('Window dressed jamb',(x,9.31,(WIN_BASE+WIN_SPRING)/2),(.28,.63,WIN_SPRING-WIN_BASE+.08),trim,.018)
for k in range(25):
    t0=k*pi/25+.004; t1=(k+1)*pi/25-.004
    poly=[(WIN_R*cos(t0),WIN_SPRING+WIN_R*sin(t0)),((WIN_R+.29)*cos(t0),WIN_SPRING+(WIN_R+.29)*sin(t0)),((WIN_R+.29)*cos(t1),WIN_SPRING+(WIN_R+.29)*sin(t1)),(WIN_R*cos(t1),WIN_SPRING+WIN_R*sin(t1))]
    extrusion('Stained glass arch stone',poly,.67,'Y',9.32,trim,.012)
box('Window sill',(0,9.17,WIN_BASE-.075),(4.78,.92,.18),trim,.03)

palette={
    'sky':[(.065,.29,.50),(.14,.43,.62),(.27,.56,.64),(.19,.37,.57)],
    'cloud':[(.69,.63,.40),(.61,.70,.65),(.80,.72,.47)],
    'hill':[(.12,.28,.27),(.19,.35,.32),(.15,.23,.35)],
    'leaf':[(.12,.27,.052),(.24,.36,.068),(.41,.36,.052),(.49,.20,.027),(.27,.15,.025)],
    'trunk':[(.115,.049,.016),(.18,.095,.022),(.27,.15,.048)],
    'water':[(.075,.25,.33),(.22,.40,.43),(.47,.53,.36)],
    'land':[(.10,.21,.065),(.24,.30,.08),(.33,.29,.092)],
    'border':[(.32,.065,.028),(.49,.23,.038),(.11,.20,.32)],
    'sun':[(.95,.63,.12),(.79,.43,.055),(.92,.76,.33)]}
glassm={}
for key,colors in palette.items():
    glassm[key]=[]
    for i,color in enumerate(colors):
        m=material('Stained glass | %s %d'%(key,i),color,.22)
        p=m.node_tree.nodes.get('Principled BSDF'); p.inputs['Transmission Weight'].default_value=.28
        p.inputs['IOR'].default_value=1.46; p.inputs['Emission Color'].default_value=(*color,1); p.inputs['Emission Strength'].default_value=.42
        n=m.node_tree.nodes.new('ShaderNodeTexNoise'); n.inputs['Scale'].default_value=38
        b=m.node_tree.nodes.new('ShaderNodeBump'); b.inputs['Strength'].default_value=.16; b.inputs['Distance'].default_value=.025
        m.node_tree.links.new(n.outputs['Fac'],b.inputs['Height']); m.node_tree.links.new(b.outputs[0],p.inputs['Normal'])
        # The glass is backlit. A mostly emissive surface keeps the colored
        # tesserae readable without a milky reflection from the large skylight.
        emit=m.node_tree.nodes.new('ShaderNodeEmission')
        emit.inputs['Color'].default_value=(*color,1); emit.inputs['Strength'].default_value=.80
        mix=m.node_tree.nodes.new('ShaderNodeMixShader'); mix.inputs[0].default_value=.10
        m.node_tree.links.new(emit.outputs[0],mix.inputs[1]); m.node_tree.links.new(p.outputs[0],mix.inputs[2])
        m.node_tree.links.new(mix.outputs[0],m.node_tree.nodes.get('Material Output').inputs['Surface'])
        glassm[key].append(m)

def glass_region(x,z):
    u=x/WIN_R; v=(z-WIN_BASE)/(WIN_TOP-WIN_BASE)
    if abs(u)>.89 or v<.045 or (z>WIN_SPRING and sqrt(x*x+(z-WIN_SPRING)**2)>WIN_R-.13): return 'border'
    if (u-.23)**2+((v-.74)*2.8)**2<.19**2: return 'sun'
    # Tree trunks and branching silhouettes.
    for tx in [-.67,.64]:
        if .18<v<.84 and abs(u-(tx+.048*sin(v*13)))<.027+.013*(1-v): return 'trunk'
        if .43<v<.81 and abs(u-(tx+(v-.49)*(-1 if tx>0 else 1)*.9))<.024: return 'trunk'
    if ((u+.62)/.48)**2+((v-.76)/.17)**2<1 or ((u-.69)/.39)**2+((v-.77)/.18)**2<1: return 'leaf'
    if v>.57:
        if abs(v-(.86+.019*sin(u*13)))<.025 or abs(v-(.65+.02*sin(u*9)))<.017: return 'cloud'
        return 'sky'
    if v>.40+.046*sin(u*6)+.028*cos(u*11): return 'hill'
    if abs(u-(.13*sin(v*14)-.06))<(.065+(.42-v)*.63): return 'water'
    return 'land'

# A true tessellated mesh with narrow gaps reveals the dark lead backing.
# Polygons are clipped to the curved aperture, leaving no rectangular corners.
poly=[(-WIN_R,WIN_BASE),(WIN_R,WIN_BASE),(WIN_R,WIN_SPRING)]
poly.extend((WIN_R*cos(t*pi/64),WIN_SPRING+WIN_R*sin(t*pi/64)) for t in range(1,65))
extrusion('Lead matrix behind individually colored panes',poly,.025,'Y',9.52,lead,0)

def clip_poly(subject, boundary):
    output=subject
    for i,p in enumerate(boundary):
        q=boundary[(i+1)%len(boundary)]; inp=output; output=[]
        if not inp: break
        def d(v): return (q[0]-p[0])*(v[1]-p[1])-(q[1]-p[1])*(v[0]-p[0])
        a=inp[-1]; da=d(a)
        for b in inp:
            db=d(b)
            if (db>=0)!=(da>=0):
                t=da/(da-db); output.append((a[0]+t*(b[0]-a[0]),a[1]+t*(b[1]-a[1])))
            if db>=0: output.append(b)
            a=b; da=db
    return output
nx,nz=30,44
points={}
for j in range(nz+1):
    for i in range(nx+1):
        points[i,j]=(-WIN_R+i*(2*WIN_R/nx)+random.uniform(-.031,.031),WIN_BASE+j*(WIN_TOP-WIN_BASE)/nz+random.uniform(-.03,.03))
for j in range(nz):
    for i in range(nx):
        corners=[points[i,j],points[i+1,j],points[i+1,j+1],points[i,j+1]]
        corners=clip_poly(corners,poly)
        if len(corners)<3: continue
        cx=sum(v[0] for v in corners)/len(corners); cz=sum(v[1] for v in corners)/len(corners)
        corners=[(cx+(x-cx)*.93,cz+(z-cz)*.93) for x,z in corners]
        mat=random.choice(glassm[glass_region(cx,cz)])
        extrusion('Glass tessera %02d.%02d'%(i,j),corners,.022,'Y',9.483,mat,.002)
# Slender stone mullions and horizontal lead support bars.
for x in [-1.29,-.43,.43,1.29]:
    h=WIN_SPRING+sqrt(WIN_R**2-x*x)-WIN_BASE
    box('Stained window stone mullion',(x,9.40,WIN_BASE+h/2),(.076,.18,h),trim,.012)
for z in [5.28,6.44,7.60,8.75]:
    w=WIN_R if z<=WIN_SPRING else sqrt(WIN_R**2-(z-WIN_SPRING)**2)
    box('Leaded horizontal saddle bar',(0,9.439,z),(w*2,.035,.031),lead,.003)

collection('08 | Oak coffered ceiling')
# Timber ceiling surrounds a genuine 4.4 x 9.3 m skylight opening.
CEIL=11.27
ceiling_x=[-7,-5.25,-3.5,-2.22,0,2.22,3.5,5.25,7]
ceiling_y=[-10,-8.25,-6.5,-4.76,-2.38,0,2.38,4.76,6.5,8.25,10]
for xa,xb in zip(ceiling_x[:-1],ceiling_x[1:]):
    for ya,yb in zip(ceiling_y[:-1],ceiling_y[1:]):
        x=(xa+xb)/2; y=(ya+yb)/2; w=xb-xa; d=yb-ya
        if abs(x)<2.22 and abs(y)<4.76: continue
        box('Recessed oak ceiling panel',(x,y,CEIL+.25),(w-.025,d-.025,.13),woodY,.012)
        for s in [-1,1]:
            box('Coffer inner moulding',(x+s*(w/2-.14),y,CEIL+.15),(.075,d-.26,.08),woodY,.014)
            box('Coffer inner moulding',(x,y+s*(d/2-.14),CEIL+.15),(w-.26,.075,.08),woodX,.014)
for x in ceiling_x:
    if abs(x)<2.2:
        for sign in [-1,1]: box('Oak longitudinal beam around skylight',(x,sign*7.36,11.12),(.22,5.12,.39),woodY,.025)
    else: box('Oak principal longitudinal beam',(x,0,11.12),(.25,20,.39),woodY,.025)
for y in ceiling_y:
    if abs(y)<4.65:
        for s in [-1,1]: box('Oak crossbeam around skylight',(s*4.69,y,11.12),(4.58,.24,.39),woodX,.025)
    else: box('Oak principal crossbeam',(0,y,11.12),(14.20,.24,.39),woodX,.025)
for x in [-6.90,6.90]: box('Oak wall plate',(x,0,11.04),(.30,20,.52),woodY,.025)
for y in [-9.88,9.30]: box('Oak end wall plate',(0,y,11.04),(14.20,.32,.52),woodX,.025)

collection('09 | Ceiling skylight and glazing')
skylightmat=material('Skylight | lightly blue antique glass',(.50,.69,.77),.15)
p=skylightmat.node_tree.nodes.get('Principled BSDF'); p.inputs['Transmission Weight'].default_value=.94; p.inputs['IOR'].default_value=1.45
for x in [-2.22,2.22]:
    box('Skylight oak curb',(x,0,11.33),(.23,9.65,.49),woodY,.027)
    box('Skylight curb trim',(x,0,11.06),(.34,9.76,.12),woodY,.019)
for y in [-4.76,4.76]:
    box('Skylight end curb',(0,y,11.33),(4.66,.23,.49),woodX,.027)
    box('Skylight end trim',(0,y,11.06),(4.86,.34,.12),woodX,.019)
for i in range(4):
    for j in range(8):
        x=-1.635+i*1.09; y=-4.13+j*1.18
        box('Skylight glass pane',(x,y,11.56),(1.057,1.147,.025),skylightmat,.006)
for i in range(5): box('Skylight lead glazing bar',(-2.18+i*1.09,0,11.57),(.047,9.46,.06),lead,.008)
for j in range(9): box('Skylight lead crossbar',(0,-4.72+j*1.18,11.58),(4.4,.047,.06),lead,.008)

collection('10 | Daylight and cameras - no fixtures')
def area(name,loc,target,energy,color,size,size_y=None):
    data=bpy.data.lights.new(name,'AREA'); data.energy=energy; data.color=color
    data.shape='RECTANGLE'; data.size=size; data.size_y=size if size_y is None else size_y
    ob=bpy.data.objects.new(name,data); COL.objects.link(ob); ob.location=loc; ob.rotation_euler=(Vector(target)-ob.location).to_track_quat('-Z','Y').to_euler(); return ob
area('Skylight daylight',(0,0,11.48),(0,0,0),1500,(.79,.88,1),4.05,8.9)
area('Stained glass daylight',(0,9.29,7.3),(0,0,1),750,(1,.80,.52),3.9,5.1)
area('Open-front soft daylight',(0,-11.8,7.5),(0,3,4),650,(.82,.87,1),9,7)
data=bpy.data.lights.new('Late-afternoon sunlight','SUN'); data.energy=1.25; data.angle=.08; data.color=(1,.86,.64)
sun=bpy.data.objects.new('Late-afternoon sunlight',data); COL.objects.link(sun); sun.rotation_euler=(.38,-.36,-.35)
world=bpy.data.worlds.new('Soft exterior sky'); world.use_nodes=True; world.node_tree.nodes['Background'].inputs[0].default_value=(.36,.43,.54,1); world.node_tree.nodes['Background'].inputs[1].default_value=.30
bpy.context.scene.world=world

def camera(name,loc,target,lens):
    d=bpy.data.cameras.new(name); ob=bpy.data.objects.new(name,d); COL.objects.link(ob); ob.location=loc; ob.rotation_euler=(Vector(target)-ob.location).to_track_quat('-Z','Y').to_euler(); d.lens=lens; d.clip_end=250; return ob
hero=camera('01 | Main hall reference view',(-1.60,-13.9,7.80),(0,2.0,5.15),23)
center=camera('02 | Centered architectural view',(0,-14.1,6.0),(0,3.1,5.4),23)
stairs_cam=camera('03 | Stair and gallery connections',(-1.2,-3.0,5.8),(0,5.6,2.55),25)
ceiling_cam=camera('04 | Timber ceiling and skylight',(0,-5.6,4.9),(0,1.9,11.0),20)

scene=bpy.context.scene; scene.camera=hero
scene.unit_settings.system='METRIC'; scene.unit_settings.length_unit='METERS'
scene.render.engine='CYCLES'; scene.cycles.samples=48; scene.cycles.use_denoising=True
scene.cycles.max_bounces=8; scene.cycles.transmission_bounces=6
scene.render.resolution_x=1500; scene.render.resolution_y=1100; scene.render.resolution_percentage=70
scene.render.image_settings.file_format='PNG'; scene.render.film_transparent=False
scene.view_settings.view_transform='AgX'
scene.view_settings.exposure=-.30
try:
    prefs=bpy.context.preferences.addons['cycles'].preferences
    for backend in ['OPTIX','CUDA','HIP','ONEAPI']:
        try:
            prefs.compute_device_type=backend; prefs.get_devices()
            gpu=[d for d in prefs.devices if d.type!='CPU']
            if gpu:
                for d in prefs.devices: d.use=(d.type!='CPU')
                scene.cycles.device='GPU'; print('RENDER DEVICES:',[(d.name,d.type,d.use) for d in prefs.devices],flush=True); break
        except Exception: pass
except Exception: pass
scene['Design brief']='Unfurnished limestone manor hall; split oak staircase; real galleries; coffered oak ceiling; stained glass and ceiling skylight.'
scene['Reference note']='Proportions inferred from a single manor-hall image. Original landscape stained-glass design made entirely as editable geometry.'
scene['Gallery finished floor (m)']=4.84
scene['Intermediate landing finished floor (m)']=LANDING
scene['Main staircase treads']=9
scene['Landing depth (m)']=1.33
scene['Main stair panelling']='Closed oak core with matching panelling on both exposed sides and a closed landing support.'
scene['Under branch panelling']=json.dumps(enclosure_data)
scene['Room inside width (m)']=14.0
scene['Room depth (m)']=19.6
scene['No furniture']=True
scene['Stair geometry']=json.dumps(stair_data)
# Save a usable starting viewport, with the hero camera and material colors.
for screen in bpy.data.screens:
    for ar in screen.areas:
        if ar.type=='VIEW_3D':
            ar.spaces.active.region_3d.view_perspective='CAMERA'
            ar.spaces.active.shading.type='MATERIAL' if False else 'SOLID'
            ar.spaces.active.shading.color_type='MATERIAL'
            ar.spaces.active.clip_end=250
bpy.ops.object.select_all(action='DESELECT')
bpy.context.view_layer.objects.active=hero; hero.select_set(True)
scene.render.filepath=os.path.join(ROOT,'renders','manor_hall_preview.png')
bpy.ops.wm.save_as_mainfile(filepath=os.path.join(ROOT,'Manor_Hall_Architecture.blend'))
print('SCENE_READY',len(bpy.data.objects),'objects',flush=True)
with open(os.path.join(ROOT,'scene_manifest.json'),'w') as f:
    json.dump({'objects':len(bpy.data.objects),'collections':[c.name for c in bpy.data.collections],'stairs':stair_data,'blender':bpy.app.version_string},f,indent=2)
if '--no-render' not in sys.argv:
    bpy.ops.render.render(write_still=True)
print('BUILD_COMPLETE',flush=True)
