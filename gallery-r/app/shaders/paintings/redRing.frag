
#version 330 core


const float MAX_TRACE_DISTANCE = .4;           // max trace distance
const float INTERSECTION_PRECISION = 0.;        // precision of the intersection
const int NUM_OF_TRACE_STEPS = 10;
const float PI  = 3.14159;

const float STEP_SIZE = MAX_TRACE_DISTANCE / float( NUM_OF_TRACE_STEPS );

in vec3 vPos;
in vec3 vEye;
in vec3 vNorm;

in vec3 vHand1;
in vec3 vHand2;

in vec2 vUV;

out vec4 color;

float hash( float n ) { return fract(sin(n)*753.5453123); }
float pNoise( in vec3 x )
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


vec3 hsv(float h, float s, float v){
  return mix( vec3( 1.0 ), clamp(( abs( fract(h + vec3( 3.0, 2.0, 1.0 ) / 3.0 )
             * 6.0 - 3.0 ) - 1.0 ), 0.0, 1.0 ), s ) * v;
}

float sdSphere( vec3 p, float s )
{
  return length(p)-s;
}

float smin( float a, float b )
{
  float k = 0.16;
  float h = clamp( 0.5 + 0.5*(b-a)/k, 0.0, 1.0 );
  return mix( b, a, h ) - k*h*(1.0-h);
}


//--------------------------------
// Modelling 
//--------------------------------
float map( vec3 pos ){
  pos.y += 0.13 ;
  pos.x += .03;

  float m = smin( sdSphere( pos , .3 ) , sdSphere( pos - vec3( 0.5 , .3 , 0. ), .3 ));
  m = smin( m , sdSphere( pos - vec3( -0.5 , .2 , 0. ), .2 ));

  return m;


}


vec4 fogStep( vec3 ro , vec3 rd ){

  vec3 p = ro;

  float lum = 0.;
  vec3 col = vec3( 0. );

  for( int i = 0; i < NUM_OF_TRACE_STEPS; i++ ){

    float id = float( i );

    p += rd * STEP_SIZE;

    float val = map( p );

    lum += val;

    col = vec3( abs(sin((id / NUM_OF_TRACE_STEPS) * 20.))  * .3 );

    if( id == 3. ){ col = vec3( 30.6 * val , 0.,  0.);}
    if( val > INTERSECTION_PRECISION ) break;



  }

  return vec4( col , lum );


}



void main(){

  vec3 ro = vPos;
  vec3 rd = normalize( vPos - vEye );


  vec3 handDir1 = normalize( vHand1 - ro);
  vec3 handDir2 = normalize( vHand2 - ro);

  vec3 col = vec3( 0. );

  vec4 fogCol = fogStep( ro , rd );

  col = fogCol.xyz;



  color = vec4( col , 1. );



}
