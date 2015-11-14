
#version 330 core
// Been loving the way this looks:
//http://static.wixstatic.com/media/06c437_f01872fb122841b16c2d1ecfeaa834f5.jpg

uniform float uTime;

in vec3 vPos;
in vec3 vEye;
in vec3 vNorm;

in vec3 vHand1;
in vec3 vHand2;

in vec2 vUV;

out vec4 color;


#define time uTime

const float INTERSECTION_PRECISION = .001;
const float MAX_TRACE_DISTANCE     = 10.;
const int NUM_TRACE_STEPS          = 30;

const float PI = 3.14159;

const vec3 lightPos = vec3( 3. , 0.  , 0. );


// This will just become vEye
vec3 lookPos;

vec3 lEyePos = vec3( -.05 , 0. , 0.0 );
vec3 rEyePos = vec3( .05 , 0. , 0.0 );

mat3 lEyeMat;
mat3 rEyeMat;
mat3 headMat;


void doCamera( out vec3 camPos , out vec3 camTar , in float time ){

  camPos = vec3( 0. , 0. , .5 );
  camTar = vec3( 0. );

}

mat3 calcLookAtMatrix( vec3 camPos , vec3 camTar , float roll ){

  vec3 up = vec3( sin( roll ) ,cos( roll ) , 0. );
  vec3 ww = normalize( camTar - camPos );
  vec3 uu = normalize( cross( ww , up ) );
  vec3 vv = normalize( cross( uu , ww ) );

  return mat3( uu , vv , ww );

}



float smin_2_3(float a, float b, float k) {
  float h = clamp(0.5 + 0.5 * (b - a) / k, 0.0, 1.0);
  return mix(b, a, h) - k * h * (1.0 - h);
}



// checks to see which intersection is closer
// and makes the y of the vec2 be the proper id
vec2 opU( vec2 d1, vec2 d2 ){
    
  return (d1.x<d2.x) ? d1 : d2;
    
}

vec2 opS( vec2 d1, vec2 d2 )
{
   return (-d1.x>d2.x) ? vec2( -d1.x , d1.y ) : d2;
}

float opS( float d1, float d2 )
{
    return max(-d1,d2);
}







float sdPlane( vec3 pos ){
 return pos.y;   
}
float sdSphere( vec3 p, float s )
{
  return length(p)-s;
}


float sdCappedCone( in vec3 p, in vec3 c )
{
    vec2 q = vec2( length(p.xy), -p.z - c.z );
    vec2 v = vec2( c.z*c.y/c.x, -c.z );

    vec2 w = v - q;

    vec2 vv = vec2( dot(v,v), v.x*v.x );
    vec2 qv = vec2( dot(v,w), v.x*w.x );

    vec2 d = max(qv,0.0)*qv/vv;

    return sqrt( dot(w,w) - max(d.x,d.y) )* sign(max(q.y*v.x-q.x*v.y,w.y));
}


float sdTorus( vec3 p, vec2 t )
{
  vec2 q = vec2(length(p.xy)-t.x,p.z);
  return length(q)-t.y;
}

float udBox( vec3 p, vec3 b )
{
  return length(max(abs(p)-b,0.0));
}

mat3 matInverse( mat3 m ){
    
  
    vec3 a = vec3(
      
        m[1][1] * m[2][2] - m[2][1] * m[1][2],
        m[0][2] * m[2][1] - m[2][2] * m[0][1],
        m[0][1] * m[1][2] - m[1][1] * m[0][2]
        
    );
    
    vec3 b = vec3(
      
        m[1][2] * m[2][0] - m[2][2] * m[1][0],
        m[0][0] * m[2][2] - m[2][0] * m[0][2],
        m[0][2] * m[1][0] - m[1][2] * m[0][0]
        
    );
    
     vec3 c = vec3(
      
        m[1][0] * m[2][1] - m[2][0] * m[1][1],
        m[0][1] * m[2][0] - m[2][1] * m[0][0],
        m[0][0] * m[1][1] - m[1][0] * m[0][1]
        
    );
    
    
    return mat3( 
        
       a.x , a.y , a.z ,
       b.x , b.y , b.z ,
       c.x , c.y , c.z
        
    );

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


vec2 smoothU( vec2 d1, vec2 d2, float k)
{
    float a = d1.x;
    float b = d2.x;
    float h = clamp(0.5+0.5*(b-a)/k, 0.0, 1.0);
    return vec2( mix(b, a, h) - k*h*(1.0-h), mix(d2.y, d1.y, pow(h, 2.0)));
}


float sdCapsule( vec3 p, vec3 a, vec3 b, float r )
{
    vec3 pa = p - a, ba = b - a;
    float h = clamp( dot(pa,ba)/dot(ba,ba), 0.0, 1.0 );
    return length( pa - ba*h ) - r;
}

float vRidge( vec3 pos ){
 
  pos -= vec3( 0. , 0.04 , .09 );
    
  pos.x = abs( pos.x );
  float r = sdCapsule( pos , vec3( 0. ,0. , 0. ) , vec3( .15 , .08 , 0. ) , .03  );
 // r = opS( sdCappedCone( pos-vec3( 0. , .03 , 0. ) , vec3( .08 , .4 , .1 ) ) , r );
    
   return r;
    
}


float sdCone( vec3 p, vec2 c )
{
    // c must be normalized
    float q = length(p.xy);
    return dot(c,vec2(q,p.z));
}


float scaledCone( vec3 p, vec2 c , float s ){
    
    return sdCone( p / s , c ) * s;
}


float beak(vec3 pos ){
   float r = scaledCone( pos , normalize( vec2( .3 , .2)) , .002);
    
   // getting rid of long tail of cylinder
   r = opS(  scaledCone( pos - vec3( 0., 0., -.05) , normalize( vec2( .1 , .2)) , .002) , r);
    
   return r;

}

float sdPlane( vec3 p, vec4 n )
{
  // n must be normalized
  return dot(p,n.xyz) + n.w;
}



vec2 map( vec3 pos ){

  vec3 pOG  = pos;
  pos -= vec3( 0. , 0.  , .5 );
  pos.z = -pos.z;



  // Drawing the Look Position
  vec2 res = vec2( 100000. , -10. );// = vec2( sdSphere( pos - lookPos , .001 ) ,0. );
    
        
  
  
  
  vec3 p;
  mat3 rot;
    
  
  p = headMat * (vec3(pOG.xy , -pOG.z + .5) - vec3( 0. , 0. ,.1));
  // Drawing Head
  res = opU( res , vec2( sdSphere( vec3(-p.xy , -p.z) + vec3( 0., 0., .1), .2) , 1. )); 
    
    
  // Getting positino for beak
  // keeping it solid on face 
  // because it makes the rest of the movement
  // seem exaggerated, and seems to make
  // the face strech and fold in really cool ways!
  rot = xrotate( .2 * PI );  
  p =  rot * ( pos ) - vec3( 0. , 0.06, .22);
    
 // res = smoothU( vec2( beak( p  ) , 2.) , res , .01);
   
    
    
  // drawing ridge
  p = headMat * (pos - vec3( 0. , 0. , .1 ));
  res = smoothU( res , vec2( vRidge( -p ) , 2. ) , .05);
    
 
  // subtracking out a conE
  rot = xrotate( .5 * PI );
  res = opS( vec2( scaledCone( rot * -p , normalize( vec2( .12 , .2)) , .002) , 1.) , res);

  
  // redraw the head
  // So we can get some of the ridge smoothed, and other parts hard
  p = headMat * (vec3(pOG.xy , -pOG.z + .5) - vec3( 0. , 0. ,.1));
  res = opU( res , vec2( sdSphere( vec3(-p.xy , -p.z) + vec3( 0., 0., .1), .2) , 1. )); 
    
  // Drawing Head
 // res = opU( res , vec2( sdSphere( vec3(-p.xy , -p.z) + vec3( 0., 0., -.1), .2) , 1. )); 
    
  // Drawing l eye
  vec3 pl = lEyeMat * (pos - lEyePos- vec3( 0. , 0. , .1 ));
    
  // Drawing r eye
  vec3 pr = rEyeMat * (pos - rEyePos- vec3( 0. , 0. , .1 ));
    
    
  res = opS( vec2( sdSphere( pl  + vec3( 0. , 0. , .1 ), .06 ) , 3. ) , res );
  res = opS( vec2( sdSphere( pr  + vec3( 0. , 0. , .1 ), .06 ) , 3. ) , res ); 
    
  res = opU( res , vec2( sdSphere( pl + vec3( 0. , 0. , .1 ) , .02 ) , 4. ));   
  res = opU( res , vec2( sdSphere( pr + vec3( 0. , 0. , .1 ) , .02 ) , 4. )); 
  
  res = opS( vec2( sdSphere( pl + vec3( 0. , 0. , .15 ) , .014 ) , 5. ),res);   
  res = opS( vec2( sdSphere( pr + vec3( 0. , 0. , .15 ) , .014 ) , 5. ), res); 
    


  res = smoothU( res , vec2( sdPlane( pos , vec4( 0. , 0., 1. , 0.3 ) ) , 1. ), .1 );
    
  return res;




}


// res = result;
vec2 calcIntersection( in vec3 ro , in vec3 rd ){

  float h     = INTERSECTION_PRECISION * 2.;
  float t     = 0.;
  float res   = -1.;
  float id    = -1.;

  for( int i = 0; i < NUM_TRACE_STEPS; i++ ){
      
    if( h < INTERSECTION_PRECISION || t > MAX_TRACE_DISTANCE ) break;
    
    vec2 m = map( ro + rd * t );
  
    h  = m.x;
    t += h;
    id = m.y;

  }

  if( t < MAX_TRACE_DISTANCE ) res = t;
  if( t > MAX_TRACE_DISTANCE ) id = -1.;

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


float softshadow( in vec3 ro, in vec3 rd, in float mint, in float tmax )
{
  float res = 1.0;
    float t = mint;
    for( int i=0; i<50; i++ )
    {
    float h = map( ro + rd*t ).x;
        res = min( res, 20.*h/t );
        t += clamp( h, 0.02, 0.10 );
        if( h<0.001 || t>tmax ) break;
    }
    return clamp( res, 0.0, 1.0 );

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






mat3 getInverseLookAtMatrix( vec3 p , vec3 lookPos , float roll ){
    
  mat3 rot = calcLookAtMatrix( vec3(p.xy , -p.z) , vec3( lookPos.xy , -lookPos.z) , 0. );
  return matInverse( rot );   
    
}

mat3 getEyeMatrix( vec3 eyePos ){
    
    return getInverseLookAtMatrix( eyePos , lookPos , 0. );
    
}


void getEyeMatrices( in vec3 lEyeP , in vec3 rEyeP , out mat3 lEye , out mat3 rEye , out mat3 hMat ){
    
    lEye = getEyeMatrix( lEyeP );
    rEye = getEyeMatrix( rEyeP );
    hMat = getEyeMatrix( vec3( 0. , 0. , .1 ) );
    
}


/*
  LETS MAKE THESE LOOK COOL!
*/

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

vec3 pal( in float t, in vec3 a, in vec3 b, in vec3 c, in vec3 d )
{
    return a + b*cos( 6.28318*(c*t+d) );
} 


vec3 cPal( float t ){
 return pal(t, vec3(0.5,0.5,0.5),vec3(0.5,0.5,0.5),vec3(2.0,1.0,0.0),vec3(0.5,0.20,0.25));   
    
}

// Need to make a good field for that to make it look like feathers ?

float radial( vec3 p ){
    return (sin( p.x ) + sin( p.y ) + sin( p.z )) * length( p.xy);
    
}

vec3 lensColor( vec3 p , vec3 n , vec3 rd ){
    
   vec3 pos; vec3 col; float v;
    
   float offset = sin(time * .1 + sin( time * .3 + sin( time * .1)));
    
   for( int i = 0; i < 3; i++ ){
       
    pos = p + rd * .1 * float(i);
    v = radial( pos * 1000. + rd * offset  );
    
    col =1. * vec3( (1. - float( i ) / 3.));//cPal( abs(v) * .1 + offset + float( i ) / 10. ) *(1.+ float( i ) / 10.);
       
    if( v > .5 ){ break;}
        
   }
    
   return col; 
    
}


vec3 doEyeHoleColor( vec3 rd, vec3 p , vec3 n , float ao , float m  ){
    
  vec3 col = vec3( 1. - ao * ao * ao ) * m;//n * .5 + .5;//lensColor( p , n , rd ) * m * m * m;
  //col = n * .5 + .5;
  return col;
    
}

vec3 doEyeColor( vec3 p , vec3 n , float ao , float m   ){
    

  vec3 col  = vec3( 1. );//vec3(ao);//vec3( 1. , 0. , 0. ) * m;
  //col = n * .5 + .5;
  return col;
    
}

vec3 doPupilColor( vec3 p , vec3 n , float ao , float m   ){
    
  vec3 col  = m *vec3( 1.4 , 0., 0.);// (n * .5 + .5);
  col = n * .5 + .5;
  return col;
    
}



vec3 doRidgeColor( vec3 p , vec3 n , float ao , float m  ){
    
  vec3 col  = vec3(m ) * vec3( 1. );// n * .5 + .5; // vec3( 1. , 1. , 1. );
  //col = n * .5 + .5;
  return col;
    
}


vec3 doFaceColor( vec3 p , vec3 n , float ao , float m   ){
    
  vec3 col  = vec3( 1. );// vec3(ao) * m ;//vec3( 0. , 0. , 1. ) * m;
  //col = n * .5 + .5;
  return col;
    
}



vec3 render( vec3 ro , vec3 rd ){
   
  vec2 res = calcIntersection( ro , rd );
  vec3 col = vec3( 1. );
  if( res.y > -.5 ){

      vec3 pos = ro + rd * res.x;
      vec3 nor = calcNormal( pos );

      float ao = calcAO( pos , nor );
      
      vec3 lightDir = lookPos - pos;
      float match = max( 0. , dot( normalize( lightDir ), nor  ));
      
      vec3 color = (nor * .5 + .5 );
      
      col = color;
      
      if( res.y == 3. ){  col = doEyeHoleColor( rd , pos , nor , ao, match ); }
      if( res.y == 4. ){  col = doEyeColor( pos , nor , ao , match); }
      if( res.y == 5. ){  col = doPupilColor( pos , nor , ao , match); }
      
      
      // Properly blending the face with the ridge
      if( res.y >= 1. && res.y <= 2. ){
          
          vec3 c1 = doFaceColor( pos , nor , ao , match);
          vec3 c2 = doRidgeColor( pos , nor , ao , match);
          
          col = c2; // mix( c1 , c2 , res.y - 1. );
          
      }
      
      
      // Coloring face and ridge
      if( res.y == 2. ){  col = doRidgeColor( pos , nor , ao , match); }
      if( res.y == 1. ){  col = doFaceColor( pos , nor , ao , match); }
    
  }
     
  return col;
    
}



void main(){



  vec3 ro = vPos;
  vec3 rd = normalize( vPos - vEye );


  vec3 handDir1 = normalize( vHand1 - ro);
  vec3 handDir2 = normalize( vHand2 - ro);

  vec2 res = calcIntersection( ro , rd );


        
  // Won't have to worry in the future about this one...
  lookPos = vEye;
    
  getEyeMatrices( lEyePos , rEyePos , lEyeMat , rEyeMat , headMat );
    

    
  //lookPos = vec3( 0. , -.3 , .5 );
 
  vec3 col = render( ro , rd );

  color = vec4( col , 1. );

}