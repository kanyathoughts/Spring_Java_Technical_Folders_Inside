/*
 * Copyright (C) 2012 tamtam180
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package innowake.mining.shared.hashing;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;

/**
 * Class to compute the hash code for a source object's content.
 */
public class CityHash {

	private static final long k0 = 0xc3a5c85c97cb3127L;
	private static final long k1 = 0xb492b66fbe98f273L;
	private static final long k2 = 0x9ae16a3b2f90404fL;
	private static final long k3 = 0xc949d7c7509e6557L;
	private static final long kMul = 0x9ddfea08eb382d69L;
	
	private CityHash() {}
	
	/**
	 * EMPTY_CONTENT_HASH.
	 */
	public static final String EMPTY_CONTENT_HASH = CityHash.cityHash128Hex(new byte[0]);
	
	/**
	 * CityHash provides hash functions for strings and returns a 128-bit hash.
	 *
	 * @param content the content to be hashed
	 * @return a 128-bit hash array as {@link String} value
	 */
	public static String cityHash128Hex(final String content) {
		return cityHash128Hex(content.getBytes());
	}
	
	/**
	 * CityHash provides hash functions for strings and returns a 128-bit hash.
	 *
	 * @param content the content to be hashed
	 * @return a 128-bit hash array as {@link String} value
	 */
	public static String cityHash128Hex(final byte[] content) {
		return cityHash128Hex(content, 0, content.length);
	}

	/**
	 * CityHash provides hash functions for strings and returns a 128-bit hash.
	 *
	 * @param content the content to be hashed
	 * @param pos the start position
	 * @param length the length of content to be hashed
	 * @return a 128-bit hash array as {@link String} value
	 */
	public static String cityHash128Hex(final byte[] content, final int pos, final int length) {
		final long[] hash = cityHash128(content, pos, length);
		return String.format("%016X%016X", hash[0], hash[1]);
	}

	/**
	 * CityHash provides hash functions for strings and returns a 64-bit hash.
	 *
	 * @param content the content to be hashed
	 * @return a 64-bit hash as hex {@link String} value
	 */
	public static String cityHash64Hex(final String content) {
		final byte[] bytes = content.getBytes();
		return cityHash64Hex(bytes, 0, bytes.length);
	}

	/**
	 * CityHash provides hash functions for strings and returns a 64-bit hash.
	 *
	 * @param content the content to be hashed
	 * @return a 64-bit hash as long
	 */
	public static long cityHash64(final String content) {
		final byte[] bytes = content.getBytes();
		return cityHash64(bytes, 0, bytes.length);
	}
	
	/**
	 * Returns a 64-bit hash value for given content.
	 *
	 * @param content the byte content to be hashed
	 * @param pos the start position
	 * @param len the length of the content
	 * @return a 64-bit hash value for the content
	 */
	public static String cityHash64Hex(final byte[] content, final int pos, final int len) {
		final long l = cityHash64(content, pos, len);
		return Long.toHexString(l);
	}

	/**
	 * Returns a 64-bit hash value for given content, by using a seed value.
	 *
	 * @param content the byte content to be hashed
	 * @param pos the start position
	 * @param len the length of the content
	 * @param seed the seed value
	 * @return a 64-bit hash value for the content
	 */
	public static String cityHash64WithSeedHex(final byte[] content, final int pos, final int len, final long seed) {
		final long l = cityHash64WithSeed(content, pos, len, seed);
		return Long.toHexString(l);
	}

	/**
	 * Returns a 64-bit hash value for given content, by using two seed values.
	 *
	 * @param content the byte content to be hashed
	 * @param pos the start position
	 * @param len the length of the content
	 * @param seed0 the first seed value
	 * @param seed1 the second seed value
	 * @return a 64-bit hash value for the content
	 */
	public static String cityHash64WithSeedsHex(final byte[] content, final int pos, final int len, final long seed0, final long seed1) {
		final long l = cityHash64WithSeeds(content, pos, len, seed0, seed1);
		return Long.toHexString(l);
	}

	private static long toLongLE(final byte[] b, final int i) {
		return (((long) b[i + 7] << 56) +
				((long) (b[i + 6] & 255) << 48) +
				((long) (b[i + 5] & 255) << 40) +
				((long) (b[i + 4] & 255) << 32) +
				((long) (b[i + 3] & 255) << 24) +
				((b[i + 2] & 255) << 16) +
				((b[i + 1] & 255) << 8) +
				((b[i + 0] & 255) << 0));
	}

	private static int toIntLE(final byte[] b, final int i) {
		return (((b[i + 3] & 255) << 24) +
				((b[i + 2] & 255) << 16) +
				((b[i + 1] & 255) << 8) +
				((b[i + 0] & 255) << 0));
	}

	private static long fetch64(final byte[] s, final int pos) {
		return toLongLE(s, pos);
	}

	private static int fetch32(final byte[] s, final int pos) {
		return toIntLE(s, pos);
	}

	private static long rotate(final long val, final int shift) {
		return shift == 0 ? val : (val >>> shift) | (val << (64 - shift));
	}

	private static long rotateByAtLeast1(final long val, final int shift) {
		return (val >>> shift) | (val << (64 - shift));
	}

	private static long shiftMix(final long val) {
		return val ^ (val >>> 47);
	}

	private static long hash128to64(final long u, final long v) {
		long a = (u ^ v) * kMul;
		a ^= (a >>> 47);
		long b = (v ^ a) * kMul;
		b ^= (b >>> 47);
		b *= kMul;
		return b;
	}

	private static long hashLen16(final long u, final long v) {
		return hash128to64(u, v);
	}

	private static long hashLen0to16(final byte[] s, final int pos, final int len) {
		if (len > 8) {
			final long a = fetch64(s, pos + 0);
			final long b = fetch64(s, pos + len - 8);
			return hashLen16(a, rotateByAtLeast1(b + len, len)) ^ b;
		}
		if (len >= 4) {
			final long a = 0xffffffffL & fetch32(s, pos + 0);
			return hashLen16((a << 3) + len, 0xffffffffL & fetch32(s, pos + len - 4));
		}
		if (len > 0) {
			final int a = s[pos + 0] & 0xFF;
			final int b = s[pos + (len >>> 1)] & 0xFF;
			final int c = s[pos + len - 1] & 0xFF;
			final int y = a + (b << 8);
			final int z = len + (c << 2);
			return shiftMix(y * k2 ^ z * k3) * k2;
		}
		return k2;
	}
	
	private static long[] weakHashLen32WithSeeds(final long w, final long x, final long y, final long z, long a, long b) {
		a += w;
		b = rotate(b + a + z, 21);
		final long c = a;
		a += x;
		a += y;
		b += rotate(a, 44);
		return new long[] {
				a + z, b + c
		};
	}

	private static long[] weakHashLen32WithSeeds(final byte[] s, final int pos, final long a, final long b) {
		return weakHashLen32WithSeeds(fetch64(s, pos + 0), fetch64(s, pos + 8), fetch64(s, pos + 16), fetch64(s, pos + 24), a, b);
	}
	

	private static long hashLen17to32(final byte[] s, final int pos, final int len) {
		final long a = fetch64(s, pos + 0) * k1;
		final long b = fetch64(s, pos + 8);
		final long c = fetch64(s, pos + len - 8) * k2;
		final long d = fetch64(s, pos + len - 16) * k0;
		return hashLen16(rotate(a - b, 43) + rotate(c, 30) + d, a + rotate(b ^ k3, 20) - c + len);
	}
	
	private static long hashLen33to64(final byte[] s, final int pos, final int len) {
		long z = fetch64(s, pos + 24);
		long a = fetch64(s, pos + 0) + (fetch64(s, pos + len - 16) + len) * k0;
		long b = rotate(a + z, 52);
		long c = rotate(a, 37);

		a += fetch64(s, pos + 8);
		c += rotate(a, 7);
		a += fetch64(s, pos + 16);

		final long vf = a + z;
		final long vs = b + rotate(a, 31) + c;

		a = fetch64(s, pos + 16) + fetch64(s, pos + len - 32);
		z = fetch64(s, pos + len - 8);
		b = rotate(a + z, 52);
		c = rotate(a, 37);
		a += fetch64(s, pos + len - 24);
		c += rotate(a, 7);
		a += fetch64(s, pos + len - 16);

		final long wf = a + z;
		final long ws = b + rotate(a, 31) + c;
		final long r = shiftMix((vf + ws) * k2 + (wf + vs) * k0);

		return shiftMix(r * k0 + vs) * k2;
	}
	
	private static long cityHash64(final byte[] s, int pos, int len) {
		if (len <= 32) {
			if (len <= 16) {
				return hashLen0to16(s, pos, len);
			} else {
				return hashLen17to32(s, pos, len);
			}
		} else if (len <= 64) {
			return hashLen33to64(s, pos, len);
		}

		long x = fetch64(s, pos + len - 40);
		long y = fetch64(s, pos + len - 16) + fetch64(s, pos + len - 56);
		long z = hashLen16(fetch64(s, pos + len - 48) + len, fetch64(s, pos + len - 24));

		long[] v = weakHashLen32WithSeeds(s, pos + len - 64, len, z);
		long[] w = weakHashLen32WithSeeds(s, pos + len - 32, y + k1, x);
		x = x * k1 + fetch64(s, pos + 0);

		len = (len - 1) & ( ~ 63);
		do {
			x = rotate(x + y + v[0] + fetch64(s, pos + 8), 37) * k1;
			y = rotate(y + v[1] + fetch64(s, pos + 48), 42) * k1;
			x ^= w[1];
			y += v[0] + fetch64(s, pos + 40);
			z = rotate(z + w[0], 33) * k1;
			v = weakHashLen32WithSeeds(s, pos + 0, v[1] * k1, x + w[0]);
			w = weakHashLen32WithSeeds(s, pos + 32, z + w[1], y + fetch64(s, pos + 16));
			{
				final long swap = z;
				z = x;
				x = swap;
			}
			pos += 64;
			len -= 64;
		} while (len != 0);

		return hashLen16(hashLen16(v[0], w[0]) + shiftMix(y) * k1 + z, hashLen16(v[1], w[1]) + x);

	}

	private static long cityHash64WithSeed(final byte[] s, final int pos, final int len, final long seed) {
		return cityHash64WithSeeds(s, pos, len, k2, seed);
	}

	private static long cityHash64WithSeeds(final byte[] s, final int pos, final int len, final long seed0, final long seed1) {
		return hashLen16(cityHash64(s, pos, len) - seed0, seed1);
	}

	private static long[] cityMurmur(final byte[] s, int pos, final int len, final long seed0, final long seed1) {

		long a = seed0;
		long b = seed1;
		long c = 0;
		long d = 0;

		int l = len - 16;
		if (l <= 0) {
			a = shiftMix(a * k1) * k1;
			c = b * k1 + hashLen0to16(s, pos, len);
			d = shiftMix(a + (len >= 8 ? fetch64(s, pos + 0) : c));
		} else {

			c = hashLen16(fetch64(s, pos + len - 8) + k1, a);
			d = hashLen16(b + len, c + fetch64(s, pos + len - 16));
			a += d;

			do {
				a ^= shiftMix(fetch64(s, pos + 0) * k1) * k1;
				a *= k1;
				b ^= a;
				c ^= shiftMix(fetch64(s, pos + 8) * k1) * k1;
				c *= k1;
				d ^= c;
				pos += 16;
				l -= 16;
			} while (l > 0);
		}

		a = hashLen16(a, c);
		b = hashLen16(d, b);

		return new long[] {
				a ^ b, hashLen16(b, a)
		};

	}

	private static long[] cityHash128WithSeed(final byte[] s, int pos, int len, final long seed0, final long seed1) {

		if (len < 128) {
			return cityMurmur(s, pos, len, seed0, seed1);
		}

		long[] v = new long[2];
		long[] w = new long[2];
		long x = seed0;
		long y = seed1;
		long z = k1 * len;

		v[0] = rotate(y ^ k1, 49) * k1 + fetch64(s, pos);
		v[1] = rotate(v[0], 42) * k1 + fetch64(s, pos + 8);
		w[0] = rotate(y + z, 35) * k1 + x;
		w[1] = rotate(x + fetch64(s, pos + 88), 53) * k1;

		do {
			x = rotate(x + y + v[0] + fetch64(s, pos + 8), 37) * k1;
			y = rotate(y + v[1] + fetch64(s, pos + 48), 42) * k1;

			x ^= w[1];
			y += v[0] + fetch64(s, pos + 40);
			z = rotate(z + w[0], 33) * k1;
			v = weakHashLen32WithSeeds(s, pos + 0, v[1] * k1, x + w[0]);
			w = weakHashLen32WithSeeds(s, pos + 32, z + w[1], y + fetch64(s, pos + 16));
			{
				final long swap = z;
				z = x;
				x = swap;
			}
			pos += 64;
			x = rotate(x + y + v[0] + fetch64(s, pos + 8), 37) * k1;
			y = rotate(y + v[1] + fetch64(s, pos + 48), 42) * k1;
			x ^= w[1];
			y += v[0] + fetch64(s, pos + 40);
			z = rotate(z + w[0], 33) * k1;
			v = weakHashLen32WithSeeds(s, pos, v[1] * k1, x + w[0]);
			w = weakHashLen32WithSeeds(s, pos + 32, z + w[1], y + fetch64(s, pos + 16));
			{
				final long swap = z;
				z = x;
				x = swap;
			}
			pos += 64;
			len -= 128;
		} while (len >= 128);

		x += rotate(v[0] + z, 49) * k0;
		z += rotate(w[0], 37) * k0;

		for (int tail_done = 0; tail_done < len;) {
			tail_done += 32;
			y = rotate(x + y, 42) * k0 + v[1];
			w[0] += fetch64(s, pos + len - tail_done + 16);
			x = x * k0 + w[0];
			z += w[1] + fetch64(s, pos + len - tail_done);
			w[1] += v[0];
			v = weakHashLen32WithSeeds(s, pos + len - tail_done, v[0] + z, v[1]);
		}

		x = hashLen16(x, v[0]);
		y = hashLen16(y + z, w[0]);

		return new long[] {
				hashLen16(x + v[1], w[1]) + y, hashLen16(x + w[1], y + v[1])
		};
	}

	private static long[] cityHash128(final byte[] s, final int pos, final int len) {
		if (len >= 16) {
			return cityHash128WithSeed(s, pos + 16, len - 16, fetch64(s, pos + 0) ^ k3, fetch64(s, pos + 8));
		} else if (len >= 8) {
			return cityHash128WithSeed(new byte[0], 0, 0, fetch64(s, pos + 0) ^ (len * k0), fetch64(s, pos + len - 8) ^ k1);
		} else {
			return cityHash128WithSeed(s, pos, len, k0, k1);
		}
	}
	
	public static byte[] cityHash128(final ByteOrder resultOrder, final byte[] content, final int pos, final int len) {
		final ByteBuffer hash = ByteBuffer.allocate(16).order(resultOrder);
		final long[] result = cityHash128(content, pos, len);
		hash.putLong(result[0]);
		hash.putLong(result[1]);
		return hash.array();
	}
	
	public static byte[] cityHash128(final ByteOrder resultOrder, final byte[] content) {
		return cityHash128(resultOrder, content, 0, content.length);
	}
	
}
