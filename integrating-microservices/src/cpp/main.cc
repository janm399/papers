#include <iostream>
#include <easylogging++.h>
#include "messages.pb.h"

INITIALIZE_EASYLOGGINGPP

const std::string fb1(const int i) noexcept {
    if (i % 3 == 0 && i % 5 == 0) return "FizzBuzz";
    else if (i % 3 == 0) return "Fizz";
    else if (i % 5 == 0) return "Buzz";
    else return std::to_string(i);
}

const std::string fb2a(const int i) noexcept {
    try {
        if (i % 3 == 0 && i % 5 == 0) throw std::runtime_error("FizzBuzz");
        else if (i % 3 == 0) return "Fizz";
        else if (i % 5 == 0) return "Buzz";
        else return std::to_string(i);
    } catch(std::exception& ex) {
        return ex.what();
    }
}

const std::string fb2b(const int i) noexcept {
    try {
        if (i % 3 == 0 && i % 5 == 0) throw std::runtime_error("FizzBuzz");
        else if (i % 3 == 0) throw std::runtime_error("Fizz");
        else if (i % 5 == 0) throw std::runtime_error("Buzz");
        else throw std::runtime_error(std::to_string(i));
    } catch(std::exception& ex) {
        return ex.what();
    }
}

template<typename T>
class condition_checker {
public:
    virtual bool check(T t) = 0;
};

const std::string fb3(const int i) noexcept {
    using cc = condition_checker<int>;
    using scc = std::shared_ptr<cc>;
    class fizz_cc : cc {
        bool check(int t) override {
            return t % 3 == 0;
        }
    };
    class buzz_cc : cc {
        bool check(int t) override {
            return t % 5 == 0;
        }
    };
    class fizzbuzz_cc : cc {
    private:
        scc is_fizz;
        scc is_buzz;
    public:
        fizzbuzz_cc(scc a_is_fizz, scc a_is_buzz) : is_fizz(a_is_fizz), is_buzz(a_is_buzz) {}

        bool check(int t) override {
            return is_fizz->check(t) && is_buzz->check(t);
        }
    };

}

int main(int argc, char** argv) {
    return 0;
}
