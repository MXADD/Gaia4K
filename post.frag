//
// (c)MX^Addict
//

#version 130

out vec4 o;
uniform sampler2D f;

vec2 iResolution = vec2(1600, 900);

// Kuwahara

void main()
{
	int    i_Radius  = 2;
	float  minSigma  = 1e+2;
	vec3   cl        = vec3(0.);
	vec2   TexelSize = 1.0 / iResolution;
	vec2   UV        = gl_FragCoord.xy*TexelSize;
	vec3   m[4],s[4];

	for (int i = 0; i < 4;i++)
		m[i]=s[i]=vec3(0);

	// 1st/2nd
	for (int j = -i_Radius; j <= 0; ++j) 
	{
		for (int i = -i_Radius; i <= 0; ++i) 
		{
			vec3 c = textureLod(f, UV + vec2(i, j) * TexelSize.xy, 0).xyz;
			m[0] += c;
			s[0] += c * c;
		}

		for (int i = 0; i <= i_Radius; ++i) 
		{
			vec3 c = textureLod(f, UV + vec2(i, j) * TexelSize.xy, 0).xyz;
			m[1] += c;
			s[1] += c * c;
		}
	}

	// 3rd/4th
	for (int j = 0; j <= i_Radius; ++j) 
	{
		for (int i = 0; i <= i_Radius; ++i) 
		{
			vec3 c = textureLod(f, UV + vec2(i, j) * TexelSize.xy, 0).xyz;
			m[2] += c;
			s[2] += c * c;
		}

		for (int i = -i_Radius; i <= 0; ++i) 
		{
			vec3 c = textureLod(f, UV + vec2(i, j) * TexelSize.xy, 0).xyz;
			m[3] += c;
			s[3] += c * c;
		}
	}

	// Final
	for (int k = 0; k < 4; ++k) 
	{
		m[k] /= ((i_Radius + 1) * (i_Radius + 1));
		s[k]  = abs(s[k] / ((i_Radius + 1) * (i_Radius + 1)) - m[k] * m[k]);

		float sigma2 = s[k].r + s[k].g + s[k].b;

		if (sigma2 < minSigma) 
		{
			minSigma = sigma2;
			cl       = m[k];
		}
	}

	// Vignetting	

	cl *= 0.4+0.6*pow(16.0*UV.x*UV.y*(1.0-UV.x)*(1.0-UV.y),0.2);

	// Output

	o = vec4(cl.x, cl.y, cl.z, 1.0);
}
