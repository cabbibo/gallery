vec2 calcIntersection( in vec3 ro, in vec3 rd , in float iPrecision , in int numSteps , in float maxDistance ){

    float h =  iPrecision * 2.0;
    float t = 0.0;
    float res = -1.0;
    float id = -1.;
    
    for( int i=0; i < numSteps ; i++ ){
        
        if( h < iPrecision || t > maxDistance ) break;
        vec2 m = map( ro+rd*t );
        h = m.x;
        t += h;
        id = m.y;
        
    }

    if( t < maxDistance ) res = t;
    if( t > maxDistance ) id =-1.0;
    
    return vec2( res , id );
     
}