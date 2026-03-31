#include <vector>
#include <cstring>
#include <cstdint>
#include <iostream>
#include <algorithm>
#include <string>
#include <sstream>
#include <unordered_map>

struct dynamic_bitset {
    dynamic_bitset() = default;
    ~dynamic_bitset() = default;
    dynamic_bitset(const dynamic_bitset &) = default;
    dynamic_bitset &operator=(const dynamic_bitset &) = default;

    dynamic_bitset(std::size_t n) { resize(n); reset(); }

    dynamic_bitset(const std::string &str) {
        resize(str.size());
        reset();
        for (std::size_t i = 0; i < str.size(); ++i) {
            if (str[i] == '1') set(i, true);
        }
    }

    bool operator[](std::size_t n) const {
        if (n >= nbits_) return false;
        std::size_t w = n >> 6U;
        std::size_t b = n & 63U;
        return (data_.empty() ? 0ULL : (data_[w] >> b) & 1ULL) != 0ULL;
    }

    dynamic_bitset &set(std::size_t n, bool val = true) {
        if (n >= nbits_) return *this;
        std::size_t w = n >> 6U;
        std::size_t b = n & 63U;
        std::uint64_t mask = 1ULL << b;
        if (val) data_[w] |= mask; else data_[w] &= ~mask;
        return *this;
    }

    dynamic_bitset &push_back(bool val) {
        std::size_t idx = nbits_;
        resize(nbits_ + 1);
        if (val) set(idx, true);
        return *this;
    }

    bool none() const {
        for (std::size_t i = 0; i + 1 < data_.size(); ++i) {
            if (data_[i] != 0ULL) return false;
        }
        if (data_.empty()) return true;
        std::uint64_t last = data_.back() & last_mask_();
        return last == 0ULL;
    }

    bool all() const {
        if (nbits_ == 0) return true; // vacuously all bits are 1? Define as true
        for (std::size_t i = 0; i + 1 < data_.size(); ++i) {
            if (data_[i] != ~0ULL) return false;
        }
        if (data_.empty()) return true;
        std::uint64_t m = last_mask_();
        return (data_.back() & m) == m;
    }

    std::size_t size() const { return nbits_; }

    dynamic_bitset &operator|=(const dynamic_bitset &o) {
        std::size_t obits = std::min(nbits_, o.nbits_);
        if (obits == 0) return *this;
        std::size_t full = obits >> 6U;
        std::size_t rem = obits & 63U;
        for (std::size_t i = 0; i < full; ++i) data_[i] |= o.get_word(i);
        if (rem) {
            std::uint64_t mask = (rem == 64 ? ~0ULL : ((1ULL << rem) - 1ULL));
            data_[full] = (data_[full] & ~mask) | ((data_[full] | (o.get_word(full))) & mask);
        }
        return *this;
    }

    dynamic_bitset &operator&=(const dynamic_bitset &o) {
        std::size_t obits = std::min(nbits_, o.nbits_);
        if (obits == 0) return *this;
        std::size_t full = obits >> 6U;
        std::size_t rem = obits & 63U;
        for (std::size_t i = 0; i < full; ++i) data_[i] &= o.get_word(i);
        if (rem) {
            std::uint64_t mask = (rem == 64 ? ~0ULL : ((1ULL << rem) - 1ULL));
            data_[full] = (data_[full] & ~mask) | ((data_[full] & (o.get_word(full))) & mask);
        }
        return *this;
    }

    dynamic_bitset &operator^=(const dynamic_bitset &o) {
        std::size_t obits = std::min(nbits_, o.nbits_);
        if (obits == 0) return *this;
        std::size_t full = obits >> 6U;
        std::size_t rem = obits & 63U;
        for (std::size_t i = 0; i < full; ++i) data_[i] ^= o.get_word(i);
        if (rem) {
            std::uint64_t mask = (rem == 64 ? ~0ULL : ((1ULL << rem) - 1ULL));
            std::uint64_t v = (data_[full] ^ (o.get_word(full))) & mask;
            data_[full] = (data_[full] & ~mask) | v;
        }
        return *this;
    }

    dynamic_bitset &operator<<=(std::size_t n) {
        if (n == 0 || nbits_ == 0) return *this;
        std::size_t old_bits = nbits_;
        std::size_t old_words = data_.size();
        std::size_t wshift = n >> 6U;
        std::size_t bshift = n & 63U;
        resize(nbits_ + n);

        // Move from high to low to avoid overwrite
        if (bshift == 0) {
            for (std::size_t i = old_words; i-- > 0;) {
                data_[i + wshift] = data_[i];
            }
        } else {
            for (std::size_t i = data_.size(); i-- > 0;) {
                std::uint64_t newv = 0ULL;
                // Source index in old data
                if (i >= wshift) {
                    std::size_t src = i - wshift;
                    if (src < old_words) newv |= (data_[src] << bshift);
                }
                if (i >= wshift + 1) {
                    std::size_t src2 = i - wshift - 1;
                    if (src2 < old_words && bshift != 0) newv |= (data_[src2] >> (64 - bshift));
                }
                data_[i] = newv;
            }
        }
        // Zero-fill the lower words introduced by shift
        for (std::size_t i = 0; i < wshift; ++i) data_[i] = 0ULL;
        // Mask high part beyond size
        mask_last_();
        return *this;
    }

    dynamic_bitset &operator>>=(std::size_t n) {
        if (n == 0 || nbits_ == 0) return *this;
        if (n >= nbits_) {
            resize(0);
            return *this;
        }
        std::size_t old_words = data_.size();
        std::size_t wshift = n >> 6U;
        std::size_t bshift = n & 63U;

        if (bshift == 0) {
            for (std::size_t i = 0; i + wshift < old_words; ++i) {
                data_[i] = data_[i + wshift];
            }
        } else {
            for (std::size_t i = 0; i < old_words; ++i) {
                std::uint64_t v = 0ULL;
                if (i + wshift < old_words) v |= (data_[i + wshift] >> bshift);
                if (i + wshift + 1 < old_words) v |= (data_[i + wshift + 1] << (64 - bshift));
                data_[i] = v;
            }
        }
        // Reduce size
        resize(nbits_ - n);
        return *this;
    }

    dynamic_bitset &set() {
        if (nbits_ == 0) return *this;
        std::fill(data_.begin(), data_.end(), ~0ULL);
        mask_last_();
        return *this;
    }

    dynamic_bitset &flip() {
        for (std::size_t i = 0; i < data_.size(); ++i) data_[i] = ~data_[i];
        mask_last_();
        return *this;
    }

    dynamic_bitset &reset() {
        std::fill(data_.begin(), data_.end(), 0ULL);
        return *this;
    }

    // Helpers
    void resize(std::size_t n) {
        nbits_ = n;
        std::size_t need = (nbits_ + 63U) >> 6U;
        data_.resize(need, 0ULL);
        if (!data_.empty()) mask_last_();
    }

    std::string to_string_lsb_first() const {
        std::string s;
        s.reserve(nbits_);
        for (std::size_t i = 0; i < nbits_; ++i) s.push_back((*this)[i] ? '1' : '0');
        return s;
    }

private:
    std::vector<std::uint64_t> data_{};
    std::size_t nbits_{0};

    std::uint64_t get_word(std::size_t i) const { return i < data_.size() ? data_[i] : 0ULL; }
    std::uint64_t last_mask_() const {
        if (nbits_ == 0) return 0ULL;
        std::size_t r = nbits_ & 63U;
        if (r == 0) return ~0ULL;
        return (1ULL << r) - 1ULL;
    }
    void mask_last_() {
        if (data_.empty()) return;
        std::uint64_t m = last_mask_();
        if (m != ~0ULL) data_.back() &= m;
    }
};

// Simple interpreter for dynamic_bitset operations.
int main() {
    std::ios::sync_with_stdio(false);
    std::cin.tie(nullptr);

    std::unordered_map<std::string, dynamic_bitset> bs;
    std::string line;
    if (!std::cin.good()) return 0;
    while (std::getline(std::cin, line)) {
        if (line.empty()) continue;
        std::istringstream iss(line);
        std::string cmd;
        // Support assignment using '=' or commands
        if (line.find("=") != std::string::npos) {
            // Format: name = <binary_string> OR name = n <size>
            std::string name, eq, rhs;
            iss >> name >> eq;
            if (!(iss >> rhs)) continue;
            // Remove quotes if any
            if (!rhs.empty() && rhs.front() == '"' && rhs.back() == '"' && rhs.size() >= 2) {
                rhs = rhs.substr(1, rhs.size() - 2);
            }
            // If rhs is digits of 0/1, init from string; else if numeric with optional suffix, treat as size
            bool bin = !rhs.empty() && rhs.find_first_not_of("01") == std::string::npos;
            if (bin) {
                bs[name] = dynamic_bitset(rhs);
            } else {
                // maybe two tokens: e.g., n 10
                if (rhs == "n") {
                    std::size_t n; iss >> n; bs[name] = dynamic_bitset(n);
                } else {
                    // try parse as number
                    std::size_t n = 0; try { n = static_cast<std::size_t>(std::stoull(rhs)); } catch (...) { n = 0; }
                    bs[name] = dynamic_bitset(n);
                }
            }
            continue;
        }

        iss >> cmd;
        if (cmd == "init") {
            std::string name; std::size_t n; iss >> name >> n; bs[name] = dynamic_bitset(n);
        } else if (cmd == "init_str") {
            std::string name, s; iss >> name >> s; if (!s.empty() && s.front()=='"' && s.back()=='"') s=s.substr(1,s.size()-2); bs[name]=dynamic_bitset(s);
        } else if (cmd == "get") {
            std::string name; std::size_t i; iss >> name >> i; std::cout << (bs[name][i] ? 1 : 0) << '\n';
        } else if (cmd == "set") {
            std::string name; std::size_t i; int v = 1; iss >> name >> i; if (iss >> v) {}; bs[name].set(i, v != 0);
        } else if (cmd == "push" || cmd == "push_back") {
            std::string name; int v; iss >> name >> v; bs[name].push_back(v != 0);
        } else if (cmd == "none") {
            std::string name; iss >> name; std::cout << (bs[name].none() ? "true" : "false") << '\n';
        } else if (cmd == "all") {
            std::string name; iss >> name; std::cout << (bs[name].all() ? "true" : "false") << '\n';
        } else if (cmd == "size") {
            std::string name; iss >> name; std::cout << bs[name].size() << '\n';
        } else if (cmd == "print") {
            std::string name; iss >> name; std::cout << bs[name].to_string_lsb_first() << '\n';
        } else if (cmd == "or" || cmd == "|=") {
            std::string a, b; iss >> a >> b; bs[a] |= bs[b];
        } else if (cmd == "and" || cmd == "&=") {
            std::string a, b; iss >> a >> b; bs[a] &= bs[b];
        } else if (cmd == "xor" || cmd == "^=") {
            std::string a, b; iss >> a >> b; bs[a] ^= bs[b];
        } else if (cmd == "shl" || cmd == "<<=") {
            std::string a; std::size_t k; iss >> a >> k; bs[a] <<= k;
        } else if (cmd == "shr" || cmd == ">>=") {
            std::string a; std::size_t k; iss >> a >> k; bs[a] >>= k;
        } else if (cmd == "ones" || cmd == "setall") {
            std::string a; iss >> a; bs[a].set();
        } else if (cmd == "flip") {
            std::string a; iss >> a; bs[a].flip();
        } else if (cmd == "reset") {
            std::string a; iss >> a; bs[a].reset();
        } else {
            // Unknown command; ignore
        }
    }
    return 0;
}

