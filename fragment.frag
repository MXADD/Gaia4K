//
// (c)MX^Addict
//

#version 130

///////////////////////////////////////////////////////////////////////////////
// Definitions & must-have functions

#define vec1  float
#define qq(x) for(vec1 zz = 0; zz++ < x;)
#define st(x) clamp(x, 0., 1.)

///////////////////////////////////////////////////////////////////////////////
// Uniforms

out vec4 o;       // Output
uniform ivec4 m;  // x - # samples since start, y - Beat, z - Slow beat history, w - Fast beat history (all beats in range 0 - 200)

///////////////////////////////////////////////////////////////////////////////
// Globals

vec1 Beat        = m.y / vec1(200);
vec1 SlowBeat    = st((m.z+m.y) / vec1(800));
vec1 iTime       = m.x / vec1(44100);
vec1 Epsilon     = 0.001;
vec1 TerrainFar  = 80.;
vec2 iResolution = vec2(1600, 900);
vec2 iUniPixels  = (2.0*gl_FragCoord.xy-iResolution.xy)/iResolution.y;
vec4 NSConst0    = vec4(0, 41, 289, 330);
vec4 NSConst1    = vec4(0.739, 0.804, 1.733, 43758.43758);

///////////////////////////////////////////////////////////////////////////////
// Common functions

// Hash 2D
vec1 hash12(vec2 x)
{
 	return fract(sin(dot(x, NSConst0.yz)) * NSConst0.w);   
}

// Fake 2D noise
vec1 n2D(vec2 p) 
{
	vec2 i = floor(p); 
         p-= i; 
         p*= p*(3. - p*2.);
    
	return dot(mat2(fract(sin(NSConst0 + dot(i, NSConst0.yz))*NSConst1.w))*vec2(1. - p.y, p.y), vec2(1. - p.x, p.x));
}

// Make 2x2 rotation matrix
mat2 r2(vec1 a)
{ 
    vec1 c = cos(a), s = sin(a); 
    return mat2(c, s, -s, c); 
}

vec2 spiralNoiseC(vec3 p)
{
    vec2 n    = vec2(0);
    vec1 iter = 1.;

	//[
	qq(8)
	//]
    {
        vec1 v = abs(sin(p.y*iter) + cos(p.x*iter)) / iter;

        n    += vec2(v, -v);
        p.xy += vec2(p.y, -p.x) * NSConst1.x;
        p.xy *= NSConst1.y;
        p.xz += vec2(p.z, -p.x) * NSConst1.x;
        p.xz *= NSConst1.y;
        iter *= NSConst1.z;
    }

    return n;
}

// Triangle noise 1D
vec1 tri(vec1 x)
{
    return abs(fract(x)-.5);
}

// Triangle noise 2D
vec2 tri2(vec2 p)
{
    return vec2(tri(p.x)+tri(p.y),tri(p.y+tri(p.x)));
}

vec2 raySphereIntersect(vec3 org, vec3 dir, vec1 r)
{
	vec1 b = dot(dir, org);
	vec1 c = dot(org, org) - (r*r);
	vec1 delta = b*b - c;
	if (delta < 0.0) 
		return vec2(-1);
	delta = sqrt(delta);
	return vec2(-delta-b, delta-b);
}

vec1 smax(vec1 a, vec1 b, vec1 s)
{
    vec1 h = st(.5 + .5*(a - b)/s);
    return mix(b, a, h) + h*(1. - h)*s;
}

///////////////////////////////////////////////////////////////////////////////
//                                 Sync fnc                                  //
///////////////////////////////////////////////////////////////////////////////

vec1 BeatColor()
{
    return clamp(st(Beat-0.1) * NSConst1.w, 1., 4.);
}

vec1 BeatPulse()
{
    return clamp((SlowBeat-.05) * 30.0, 0., .4);
}

///////////////////////////////////////////////////////////////////////////////
//                                  Effects                                  //
///////////////////////////////////////////////////////////////////////////////
// Letter logos EFX

vec1 letterLineSDF(vec2 p, vec4 c)
{
    p    -= c.xy;
    c.zw -= c.xy; 
    return length((p) - (c.zw) * st(dot(p, c.zw) / dot(c.zw, c.zw)));
}

vec1 letterArcSDF(vec2 p, vec4 a)
{
    int  w = int(a.w);
	int  m = w - w / 2 * 2;
         p-= a.xy;
	return st(dot(sign(p), vec2(m, m < 1) * vec2(w / 2 - 1))) + abs(length(p) - a.z);
}

vec1 letterD(vec2 o)
{
    return min(min(letterLineSDF(o, vec4(2,1,2,8)), letterLineSDF(o, vec4(1,1,4,1))), min(letterLineSDF(o, vec4(1,9,4,9)), letterArcSDF(o, vec4(4,5,4,1))));
}

vec1 letterA(vec2 o)
{
    return min(min(letterLineSDF(o, vec4(1,1,5,9)), letterLineSDF(o, vec4(5,9,9,1))), letterLineSDF(o, vec4(3,4,7,4)));
}

vec1 letterI(vec2 o)
{
    return min(min(letterLineSDF(o, vec4(3,1,3,9)), letterLineSDF(o, vec4(2,1,4,1))), letterLineSDF(o, vec4(2,9,4,9)));
}

vec1 letterC(vec2 o)
{
    return min(min(letterLineSDF(o, vec4(1,9,2,9)), letterLineSDF(o, vec4(1,1,2,1))), letterArcSDF(o, vec4(1,5,4,5)));
}

vec4 lettersColor(vec1 f)
{
    vec1 g = st((.2 / f)-.2) * (sin(iTime * 4.) * .2 + .8);
    return vec4(g, g * .5, 0., 1.) + st((.05 / f)-.5);
}

vec4 traceAddict()
{
    vec2  o = (iUniPixels - vec2(-.85, -.2)) * vec2(30., 20.);
	
	// A
    vec1 f = min(1., letterA(o));

	// D             
    o.x -= 10.;       
    f = min(f, letterD(o));
                     
	// D             
    o.x -= 10.;       
    f = min(f, letterD(o));
                     
	// I             
    o.x -= 10.;      
    f = min(f, letterI(o));
                     
	// C             
    o.x -= 10.;       
    f = min(f, letterC(o));
                     
	// T (inlined due to single usage)
    o.x -= 2.;  
    return lettersColor(min(f, min(letterLineSDF(o, vec4(5,1,5,9)), letterLineSDF(o, vec4(1,9,9,9)))));
}

vec4 traceGaia()
{
    vec2  o = (iUniPixels - vec2(-.4, -.2)) * vec2(30., 20.);

	// G (C + additional 2 lines)
    vec1 f = min(1., letterC(o));
    f = min(f, min(letterLineSDF(o, vec4(2,1,2,5)), letterLineSDF(o, vec4(2,5,1,5))));

	// A
    o.x -= 4.;
    f = min(f, letterA(o));

	// I
    o.x -= 10.;
    f = min(f, letterI(o));

	// A
    o.x -= 5.;
    return lettersColor(min(f, letterA(o)));
}

///////////////////////////////////////////////////////////////////////////////
// Nebula EFX

vec1 mapNebula(vec3 p) 
{
    p.xz *= r2(iTime * 0.12);
    vec2 ns = spiralNoiseC(p*2.);
    return abs((p.y + 5.) - ns.y - ns.x + (spiralNoiseC(p.zxy+100.0).y*5.))*0.5+0.03;
}

vec3 computeNebulaColor(vec1 density, vec1 radius)
{
	return mix(vec3(1.0,0.9,0.8), vec3(0.40,0.15,0.10), density) * mix(vec3(6.), vec3(.9), min((radius+.05)/.9, 1.15));
}

vec3 traceNebulaField(vec3 ro, vec3 rd, vec2 dist)
{
    vec4 sum = vec4(.0);
	vec1 ld=0., td=0.;

    vec1 t = dist.x * step(0, dist.x);
   
	//[
	qq(64)
	//]
    {
        if (td > 0.9 || t > 10. || sum.a > 0.99 || t > dist.y) 
            break;

        vec3 pos     = ro + t*rd;
        vec1 d       = max(mapNebula(pos), 0.08);
        vec1 density = length(pos);

        sum.rgb += (vec3(1.1, 0.5, 0.33)/(density*density)/30.); // Color multiplied by square density and some factor
      
		if (d < .1) 
		{
    	    ld  = .1 - d;
		    td += ((1. - td) * ld) + 1./200.;
   			sum = sum + (vec4(computeNebulaColor(td, density), 1) * td * 0.185) * (1.0 - sum.a);  
		}
      
		td += 1./70.;
        d   = d*(.8+0.2*hash12(iUniPixels+iTime+density*200.));
        t  += max(d * 0.1 * clamp(density, 1.0, length(ro)), 0.02);
	}
    
    sum = st(sum * (1./exp(ld*0.2) * 0.6));
    return (sum*sum*(3.0-2.0*sum)).xyz;
}

vec4 traceNebula(vec1 off)
{  
    // Ray setup

    vec1  zo  = mix(7.0, 0.5, st((iTime-off)/30.0)) - BeatPulse(); // Zoom over time
    vec3  rd  = normalize(vec3(iUniPixels, 1.));
	vec3  ro  = vec3(0., 0., -zo);
    vec4  sum = vec4(raySphereIntersect(ro, rd, 3.), 0, 0);

    // Trace inside sphere

    if (sum.y>0)
        sum.xyz = traceNebulaField(ro, rd, sum.xy);

    // Some saturation flickering

    return st(mix(sum*sum, sum, .5 + .5 * abs(sin(iTime * 1.5))) * BeatColor());
}

///////////////////////////////////////////////////////////////////////////////
// Alien planet EFX

vec2 terrainCanyonShape(vec1 z)
{ 
    return vec2((sin(z*.25)*3.)+(cos(z*.08)*8.), sin(z*.1)*.7);
}

vec1 terrain(vec2 p)
{
    vec1 a = 1., sum = 0., res = 0.;

	//[
	qq(8)
	//]
    {
        res += abs(n2D(p) - .5)*a;
        p    = mat2(1, -.75, .75, 1)*p*2.75;
        sum += a;
        a   *= -.5/1.75; // NOTE: -0.5/2.0 also looks cool!
    }
   
    return res/sum;
}

vec1 terrainMap(vec3 p)
{
    vec1 trSf = terrain(p.xz * 0.125 + 0.5);

    p.xy   -= terrainCanyonShape(p.z);
    vec2 ca = abs(p.xy*vec2(1, .7) + vec2(0, -2.75));

    return (smax(6. - mix(length(ca), max(ca.x, ca.y), .25), p.y - 1.75, 2.) + (.5 - trSf)*4.)*.75;
}

vec1 terrainTrace(vec3 ro, vec3 rd)
{
    vec1 t = 0., d;

	//[
	qq(160)
	//]
    {
        d = terrainMap(ro + rd*t);
        if (abs(d) < Epsilon || t > TerrainFar) 
            break;
        
        t += d;
    }
    
    return min(t, TerrainFar);
}

vec1 terrainSoftShadow(vec3 ro, vec3 rd, vec1 k, vec1 t)
{
    vec1 shade = 1.;
    vec1 dist  = Epsilon*(t*.125 + 1.);
    vec1 end   = max(length(rd), Epsilon); // WAS:0.0001 -- changed for better compression!
          rd  /= end;

	//[
	qq(64)
	//]
    {
        vec1 h = terrainMap(ro + rd*dist);
        shade  = min(shade, smoothstep(0.0, 1.0, k*h/dist));
        dist  += clamp(h, .02, .25); 

        if (h < 0. || dist > end) 
            break; 
    }

    return clamp(shade, .15, 1.);
}

vec3 terrainNormal(vec3 p)
{
    vec2 e = vec2(0.002, -0.002); 
    return normalize(e.xyy*terrainMap(p + e.xyy) + e.yyx*terrainMap(p + e.yyx) + e.yxy*terrainMap(p + e.yxy) + e.xxx*terrainMap(p + e.xxx));
}

vec1 terrainAO(vec3 p, vec3 nor)
{
	vec1 sca = 1.5, occ = 0., hr=.01;

	//[
	qq(8)
	//]
    {
        occ += (hr - terrainMap(nor*hr + p))*sca;
        sca *= .7;
        hr  += 0.125;
    }
    
    return st(1. - occ);
}

vec1 terrainFbmAurora(vec2 p) 
{
    vec3 fct = vec3(2., 2.5, 0.);
         p *= r2(p.x * 0.05);
    vec2 bp = p;

	//[
	qq(8)
	//]
    {
        vec2 dg = (tri2(bp*2.0)*.75) * r2(iTime*0.05);
             p -= dg/fct.y;

        bp     *= 1.5;
        fct.xy *= vec2(.45, .5);
		p      *= 1.2 + (fct.z-1.0)*.02;
        
        fct.z  += tri(p.x+tri(p.y))*fct.x;
        p      *= sin(iTime * 0.05) * cos(iTime * 0.01);
	}

    return st(1. / pow(fct.z * 20., 1.5));
}

vec3 terrainAurora(vec3 rd, vec2 seed) 
{
    vec3 col = vec3(0), avgCol = col;

    //[
    qq(64)
    //]
    {
        avgCol = (avgCol + ((sin(1.-vec3(2.15,-.5, 1.4) + zz * 0.04) * 0.5 + 0.5)*terrainFbmAurora((5.5 + ((((.8+pow(zz,1.4)*.002)) / (rd.y * 2. + 0.4)) - (0.005*hash12(seed/*gl_FragCoord.xy*/)*smoothstep(0.,15., zz))) * rd).zx))) * .5;
        col   += avgCol * exp2(-zz*0.065 - 2.5) * smoothstep(0., 5., zz);
    }

    return smoothstep(0.,1.1,col*1.5*st(rd.y*15.+.5)).yzx;
}

vec3 terrainSkyGradient(vec3 rd)
{
    // Just horizontal gradient

    vec1 h = 1.0 - st(rd.y+0.1);
    return mix(vec3(0.10, .35, .25), vec3(.05, .15, .20), h*h);
}

vec3 terrainObjectColor(vec3 p, vec3 n)
{
    // Object texture color.

    vec3 tx = vec3(.5,.4,.3);

    // Trick to get some green tint where the surface is flat (looks like some vegetation)

    return mix(tx, tx*(mix(vec3(1), vec3(.8, 1.3, .2), smoothstep(.5, 1., n.y))), smoothstep(.7, 1., (n.y)));    
}

vec3 terrainDoColor(vec3 ro, vec3 rd, vec1 t)
{
    vec3 ld = vec3(0.33, 0.66, 0.44); // Normalized light direction
    vec3 sp = ro + rd*t;
    vec3 sn = terrainNormal(sp);

    // Shading. Shadows, ambient occlusion, etc.
    vec1 sh = terrainSoftShadow(sp + sn*.002, ld, 16., t);
    vec1 ao = terrainAO(sp, sn);
         sh = (sh + ao * .25) * ao;
        
    return terrainObjectColor(sp, sn) * ((vec3(0.25)+vec3(0.8,0.9,0.8)*st(dot(sn, ld))) + ao *.5 + vec3(0.3, .7, 0.8) * pow(st(dot(reflect(-ld, sn), -rd)), 32.0)) * sh * max(1.0, BeatColor() * .5);
}

vec3 terrainFullTrace(vec3 ro, vec3 rd, vec2 uv)
{
    vec1 t = terrainTrace(ro, rd);
    return  mix(t < TerrainFar ? terrainDoColor(ro, rd, t) : vec3(0), terrainSkyGradient(rd) + (terrainAurora(rd, uv) * BeatColor()), smoothstep(0., .95, t/TerrainFar));
}

vec1 metaballsCalcDF(vec3 pos)
{
    vec1 bp = BeatPulse() * 4.;
    vec1 ec = mix(1.2, 2.0, st(bp));
	vec1 d1 = length(pos - ec * vec3(cos(iTime*1.1),cos(iTime*1.3),cos(iTime*1.7)))-mix(.3, .6, bp);
    vec1 d2 = length(pos + ec * vec3(cos(iTime*0.7),cos(iTime*1.9),cos(iTime*2.3)))-mix(.2, .4, bp);
    vec1 d3 = length(pos + ec * vec3(cos(iTime*0.3),cos(iTime*2.9),sin(iTime*1.1)))-mix(.3, .6, bp);
    vec1 d4 = length(pos + ec * vec3(sin(iTime*1.3),sin(iTime*1.7),sin(iTime*0.7)))-mix(.3, .6, bp);
    vec1 d5 = length(pos + ec * vec3(sin(iTime*2.3),sin(iTime*1.9),sin(iTime*2.9)))-mix(.2, .5, bp);
    
    return -log(exp(-d1)+exp(-d2)+exp(-d3)+exp(-d4)+exp(-d5));
}

vec1 metaballsCalcScene(vec3 ro, vec3 rd)
{
    vec1 t = 0.0, h;

	//[
	qq(64)
	//]
    {
	    h = metaballsCalcDF(ro+rd*t);
        t += h;

        if (h < Epsilon)
            break;

        if (t > 32.0)
            return -1.0;
    }

    return t;
}

vec3 metaballsCalcNormal(vec3 pos )
{
    vec2 v = vec2(-1.,1.);

	return normalize(v.yxx*metaballsCalcDF(pos + v.yxx*Epsilon) + 
					 v.xxy*metaballsCalcDF(pos + v.xxy*Epsilon) + 
					 v.xyx*metaballsCalcDF(pos + v.xyx*Epsilon) + 
					 v.yyy*metaballsCalcDF(pos + v.yyy*Epsilon) );
}

vec4 TraceAlienPlanet()
{
	// Camera setup

	vec3 ro = vec3(0, 0, iTime*5.);
	vec3 lk = ro + vec3(0, -.04, .25);

    // Translate into canyon

    ro.xy += terrainCanyonShape(ro.z);
	lk.xy += terrainCanyonShape(lk.z);
    
    // Using the above to produce the unit ray-direction vector.

    vec2 uv      = iUniPixels * 0.75; // FOV
    vec3 forward = normalize(lk-ro);
    vec3 right   = normalize(vec3(forward.z, 0., -forward.x )); 
    vec3 up      = cross(forward, right);
    vec3 rd      = normalize(forward + uv.x*right + uv.y*up);
    
    // Camera - based on path position

    vec2 sw = terrainCanyonShape(lk.z);

    rd.xy *= r2(-sw.x/24.);
    rd.yz *= r2(-sw.y/16.);
    
    // Trace the scene.    

    vec3 sceneColor = terrainFullTrace(ro, rd, uv);

    // Custom metaballs mix wit nebula inside
    // Metaballs are contained within sphere to speed up things
    
    vec3 spo = ro + forward * mix(9., 2., st((iTime-57.0)*0.5));
    vec2 tmm = raySphereIntersect(ro-spo, rd, 4.5);

    if (tmm.y > 0)
    {
        tmm.y = metaballsCalcScene(ro-spo, rd);
        if (tmm.y > 0)
        {
            vec3 rp  = (ro-spo) + tmm.y*rd;
            vec3 nm  = metaballsCalcNormal(rp);
                 rp += spo;

            tmm.y = metaballsCalcScene((ro-spo)+(rd*32.0), -rd); // Trace end (trace from opposite direction)
            rd    = refract(rd, nm, 0.93);

            sceneColor = mix(sceneColor, mix(traceNebulaField((ro-spo)*(0.4 + BeatPulse()), rd, tmm*vec2(.5, 2.)) * BeatColor(), terrainFullTrace(rp, reflect(rd, nm), uv)*1.5, pow(1.-(abs(dot(nm,rd))), 1.5)), 0.9);
        }
    }
         
    return vec4(sceneColor, 1.);
}

///////////////////////////////////////////////////////////////////////////////
// Main

void main()
{
	vec4 Nebula = o = traceNebula(iTime > 33. ? 20.0 : 0.0);
	vec4 AlienPlanet = (iTime < 30.) ? Nebula : TraceAlienPlanet();

    // Main stages

    o = mix(o, AlienPlanet, st((iTime-30.)*.7));
    o = mix(o, Nebula,      st((iTime-43.0)*2.0));
    o = mix(o, AlienPlanet, st((iTime-45.0)*3.0));
    o = mix(o, Nebula,      st((iTime-58.0)*2.0));
    o = mix(o, vec4(0.),    st((iTime-60.)*.50));

    // Logos
	if (iTime < 23.)
	{
		o+= traceAddict() * (1.0 - BeatPulse()) * mix(st(iTime- 2.0), 0, st((iTime- 7.)*.5));
		o+= traceGaia() * BeatColor() * mix(st(iTime-15.0), 0, st((iTime-20.)*.5));
	}
}

///////////////////////////////////////////////////////////////////////////////
