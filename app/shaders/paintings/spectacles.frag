#version 330 core

uniform float uTime;

const float MAX_TRACE_DISTANCE = 4.;           // max trace distance
const float INTERSECTION_PRECISION = 0.001;        // precision of the intersection
const int NUM_OF_TRACE_STEPS = 100;
const float PI  = 3.14159;




in vec3 vPos;
in vec3 vEye;
in vec3 vNorm;

in vec3 vHand1;
in vec3 vHand2;

in vec2 vUV;

out vec4 color;

vec3 hsv(float h, float s, float v)
{
  return mix( vec3( 1.0 ), clamp( ( abs( fract(
    h + vec3( 3.0, 2.0, 1.0 ) / 3.0 ) * 6.0 - 3.0 ) - 1.0 ), 0.0, 1.0 ), s ) * v;
}

// ROTATION FUNCTIONS TAKEN FROM
//https://www.shadertoy.com/view/XsSSzG
mat3 xrotate(float t) {
  return mat3(1.0, 0.0, 0.0,
                0.0, cos(t), -sin(t),
                0.0, sin(t), cos(t));
}

mat3 yrotate(float t) {
  return mat3(cos(t), 0.0, -sin(t),
                0.0, 1.0, 0.0,
                sin(t), 0.0, cos(t));
}

mat3 zrotate(float t) {
    return mat3(cos(t), -sin(t), 0.0,
                sin(t), cos(t), 0.0,
                0.0, 0.0, 1.0);
}


float sdSphere( vec3 p, float s ){
  return length(p)-s;
}

float opRepSphere( vec3 p, vec3 c , float r)
{
    vec3 q = mod(p,c)-0.5*c;
    return sdSphere( q  , r  );
}


vec2 smoothU( vec2 d1, vec2 d2, float k)
{
    float a = d1.x;
    float b = d2.x;
    float h = clamp(0.5+0.5*(b-a)/k, 0.0, 1.0);
    return vec2( mix(b, a, h) - k*h*(1.0-h), mix(d2.y, d1.y, pow(h, 2.0)));
}

float sdBox( vec3 p, vec3 b )
{
  vec3 d = abs(p) - b;
  return min(max(d.x,max(d.y,d.z)),0.0) +
         length(max(d,0.0));
}

float opU( float d1, float d2 ){
  return min(d1,d2);
}

vec2 opU( vec2 d1, vec2 d2 )
{
    return  d1.x < d2.x ? d1 : d2 ;
}

float opS( float d1, float d2 )
{
    return max(-d1,d2);
}

float sdCylinder( vec3 p, vec3 c )
{
  return length(p.xy-c.xy)-c.z;
}


float sdCappedCylinder( vec3 p, vec2 h )
{
    
    
  vec2 d = abs(vec2(length(p.xz),p.y)) - h;
  return min(max(d.x,d.y),0.0) + length(max(d,0.0));
}



float sdTorus( vec3 p, vec2 t )
{
  vec2 q = vec2(length(p.xz)-t.x,p.y);
  return length(q)-t.y;
}




vec2 glasses( vec3 p ){
    
    
   vec3 og = p;
    
   mat3 r = xrotate( -.18 * PI / 2. );
    
    p = r * p;
 
    
   p.x = abs( p.x );
    
   vec3 eyePos = vec3( 0.25 , 0. , 0. );
    
   r = xrotate( PI / 2. );
    
   vec2 res;
    
   // Rims
   res = vec2( sdTorus( r * (p -eyePos) , vec2( .16 , .02 ) ) , 2. );
    
    
   //spectacles
   res = smoothU( res ,vec2( sdCappedCylinder( r * (p -eyePos) , vec2( .15 , .01 ) ), 10. ), .01);
    
    
   eyePos = vec3( 0.41 , 0.1 , -0.25 );
    
   //earHolders
   res = smoothU( res ,vec2( sdCappedCylinder( r * (p -eyePos) , vec2( .01 , .25 ) ), 2. ), .03);
    
   r = xrotate(.6* PI / 2. );
   eyePos = vec3( 0.41 , 0.06 , -0.55 );
   res = smoothU( res ,vec2( sdCappedCylinder( r * (p -eyePos) , vec2( .008 , .08 ) ), 3. ), .02);
     
    
   // cross bar
    eyePos = vec3( 0. , .13 , 0. );
    
   r = zrotate( PI / 2. );
    
   res = smoothU( res , vec2( sdCappedCylinder( r * ( og - eyePos) , vec2( .01 , .13 )) , 2. ), .03);
   return res;
    
}

float sdPlane( vec3 p, vec4 n )
{
  // n must be normalized
  return dot(p,n.xyz) + n.w;
}


//--------------------------------
// Modelling 
//--------------------------------
vec2 map( vec3 pos ){  
   
    pos -= vec3( 0., 0. , .8);
    pos.z = -pos.z;

    pos = yrotate( 1. ) * pos;
    vec2 ring;


    vec2 res = glasses( pos );
    vec2 plane = vec2( sdPlane( pos , vec4( 0., 1. , 0. , .17 ) ), 20. );
    
    res = opU( res , plane );
    

    
      
    return res;
    
}



vec2 calcIntersection( in vec3 ro, in vec3 rd ){

    float h =  INTERSECTION_PRECISION * 2.0;
    float t = 0.0;
    float res = -1.0;
    float id = -1.;
    
    for( int i=0; i < NUM_OF_TRACE_STEPS ; i++ ){
        
        if( h < INTERSECTION_PRECISION || t > MAX_TRACE_DISTANCE ) break;
        vec2 m = map( ro+rd*t );
        h = m.x;
        t += h;
        id = m.y;
        
    }

    if( t < MAX_TRACE_DISTANCE ) res = t;
    if( t > MAX_TRACE_DISTANCE ) id =-1.0;
    
    return vec2( res , id );
     
}

// Calculates the normal by taking a very small distance,
// remapping the function, and getting normal for that
vec3 calcNormal( in vec3 pos ){
    
  vec3 eps = vec3( 0.001, 0.0, 0.0 );
  vec3 nor = vec3(
      map(pos+eps.xyy).x - map(pos-eps.xyy).x,
      map(pos+eps.yxy).x - map(pos-eps.yxy).x,
      map(pos+eps.yyx).x - map(pos-eps.yyx).x );

  return normalize(nor);
}


float calcAO( in vec3 pos, in vec3 nor )
{
  float occ = 0.0;
    float sca = 1.0;
    for( int i=0; i<5; i++ )
    {
        float hr = 0.01 + 0.612*float(i)/4.0;
        vec3 aopos =  nor * hr + pos;
        float dd = map( aopos ).x;
        occ += -(dd-hr)*sca;
        sca *= 0.5;
    }
    return clamp( 1.0 - 3.0*occ, 0.0, 1.0 );    
}

float hash( float n ) { return fract(sin(n)*753.5453123); }
float noise( in vec3 x )
{
    vec3 p = floor(x);
    vec3 f = fract(x);
    f = f*f*(3.0-2.0*f);
  
    float n = p.x + p.y*157.0 + 113.0*p.z;
    return mix(mix(mix( hash(n+  0.0), hash(n+  1.0),f.x),
                   mix( hash(n+157.0), hash(n+158.0),f.x),f.y),
               mix(mix( hash(n+113.0), hash(n+114.0),f.x),
                   mix( hash(n+270.0), hash(n+271.0),f.x),f.y),f.z);
}


vec3 cPal( float t ){
 return vec3( 1. - t );    
}
vec3 lensColor( vec3 p , vec3 n , vec3 rd ){
    
   vec3 pos; vec3 col; float v;
    
   float offset = sin(uTime * .1 + sin( uTime * .3 + sin( uTime * .1)));
    
   for( int i = 0; i < 10; i++ ){
       
    pos = p + rd * .03 * float(i);
    v = noise( pos * 10. + rd * offset  );
    
    col = hsv( abs(v) * .1 + offset + float( i ) / 10. , 1. , 1. ) *(1.+ float( i ) / 10.);
       
    if( v > .5 ){ break;}
        
   }
    
   return col; 
    
}

vec3 turtleColor( vec3 p , vec3 n ){
    
   return vec3( 1. );
}

vec3 planeColor( vec3 p , vec3 n  , float ao ){
    
   
   return cPal( length( p ) * .1 ) *ao;
   
}

vec3 bgColor(){
    
   return vec3( .8 );
}


void main(){

  vec3 ro = vPos;
  vec3 rd = normalize( vPos - vEye );


  vec3 handDir1 = normalize( vHand1 - ro);
  vec3 handDir2 = normalize( vHand2 - ro);

  vec2 res = calcIntersection( ro , rd );


  vec3 col = bgColor();


  if( res.y > .5 ){

    vec3 pos = ro + rd * res.x;

    vec3 handDir1 = normalize( vHand1 - pos);
    vec3 handDir2 = normalize( vHand2 - pos);
    vec3 norm;

    
    norm = calcNormal( pos );

    float ao = calcAO( pos , norm );
        
      
        
    vec3 refr = refract( rd , norm , 1. / 1.2 );
    
    //col = textureCube( iChannel0 , normalize(refr) ).xyz;


    // Lens
    if( res.y >= 4. && res.y < 10.1 ){
      col = lensColor( pos , norm , rd );// vec3( pow((1. - dot( norm , normalize( pos ) )) , .3) );
    
    // Plane
    }else if( res.y == 20. ){
      col = planeColor( pos , norm , ao ); 
        
    //Turtle
    }else if( res.y== 3. ){
      col = vec3( 2.  ) * ao;   
        
    // body
    }else{
      col =  vec3( .5 ) * ao;
    }



  }




  color = vec4( col , 1. );



}