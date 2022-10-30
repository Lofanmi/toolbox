package gotil

import (
	"crypto/md5"
	"crypto/sha1"
	"crypto/sha256"
	"crypto/sha512"
	"encoding/hex"
	"hash/crc32"
	"hash/fnv"
	"strings"
)

func MD5(str string, upper bool) string {
	h := md5.New()
	_, _ = h.Write([]byte(str))
	result := hex.EncodeToString(h.Sum(nil))
	if upper {
		return strings.ToUpper(result)
	}
	return result
}

func CRC32(str string, upper bool) string {
	h := crc32.NewIEEE()
	_, _ = h.Write([]byte(str))
	result := hex.EncodeToString(h.Sum(nil))
	if upper {
		return strings.ToUpper(result)
	}
	return result
}

func SHA1(str string, upper bool) string {
	h := sha1.New()
	_, _ = h.Write([]byte(str))
	result := hex.EncodeToString(h.Sum(nil))
	if upper {
		return strings.ToUpper(result)
	}
	return result
}

func SHA256(str string, upper bool) string {
	h := sha256.New()
	_, _ = h.Write([]byte(str))
	result := hex.EncodeToString(h.Sum(nil))
	if upper {
		return strings.ToUpper(result)
	}
	return result
}

func SHA512(str string, upper bool) string {
	h := sha512.New()
	_, _ = h.Write([]byte(str))
	result := hex.EncodeToString(h.Sum(nil))
	if upper {
		return strings.ToUpper(result)
	}
	return result
}

func FNV32(str string, upper bool) string {
	h := fnv.New32()
	_, _ = h.Write([]byte(str))
	result := hex.EncodeToString(h.Sum(nil))
	if upper {
		return strings.ToUpper(result)
	}
	return result
}

func FNV64(str string, upper bool) string {
	h := fnv.New64()
	_, _ = h.Write([]byte(str))
	result := hex.EncodeToString(h.Sum(nil))
	if upper {
		return strings.ToUpper(result)
	}
	return result
}

func FNV128(str string, upper bool) string {
	h := fnv.New128()
	_, _ = h.Write([]byte(str))
	result := hex.EncodeToString(h.Sum(nil))
	if upper {
		return strings.ToUpper(result)
	}
	return result
}
