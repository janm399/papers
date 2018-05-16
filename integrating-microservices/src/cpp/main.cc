#include <iostream>
#include <chrono>
#include <easylogging++.h>
#include <future>
#include "messages.pb.h"

using namespace std;
using namespace std::chrono;

INITIALIZE_EASYLOGGINGPP

const string fb1(const int i) noexcept {
    if (i % 3 == 0 && i % 5 == 0) return "FizzBuzz";
    else if (i % 3 == 0) return "Fizz";
    else if (i % 5 == 0) return "Buzz";
    else return to_string(i);
}

const string fb2a(const int i) noexcept {
    try {
        if (i % 3 == 0 && i % 5 == 0) throw runtime_error("FizzBuzz");
        else if (i % 3 == 0) return "Fizz";
        else if (i % 5 == 0) return "Buzz";
        else return to_string(i);
    } catch(exception& ex) {
        return ex.what();
    }
}

const string fb2b(const int i) noexcept {
    try {
        if (i % 3 == 0 && i % 5 == 0) throw runtime_error("FizzBuzz");
        else if (i % 3 == 0) throw runtime_error("Fizz");
        else if (i % 5 == 0) throw runtime_error("Buzz");
        else throw runtime_error(to_string(i));
    } catch(exception& ex) {
        return ex.what();
    }
}

template<typename T>
class condition_checker {
public:
    virtual bool check(T t) = 0;
};

const string fb3(const int i) noexcept {
    using cc = condition_checker<int>;
    using scc = shared_ptr<cc>;
    class fizz_cc : public cc {
    public:
        bool check(int t) override {
            return t % 3 == 0;
        }
    };
    class buzz_cc : public cc {
    public:
        bool check(int t) override {
            return t % 5 == 0;
        }
    };
    class fizzbuzz_cc : public cc {
    private:
        scc is_fizz;
        scc is_buzz;
    public:
        fizzbuzz_cc(scc a_is_fizz, scc a_is_buzz) : is_fizz(a_is_fizz), is_buzz(a_is_buzz) {}

        bool check(int t) override {
            return is_fizz->check(t) && is_buzz->check(t);
        }
    };
    auto fizz = make_shared<fizz_cc>();
    auto buzz = make_shared<buzz_cc>();
    auto fizz_buzz = fizzbuzz_cc(fizz, buzz);

    if (fizz_buzz.check(i)) return "FizzBuzz";
    else if (fizz->check(i)) return "Fizz";
    else if (buzz->check(i)) return "Buzz";
    else return std::to_string(i);
}

const string fb4(const int i) noexcept {
    return async(fb1, i).get();
}

template<typename F>
long run(F f) {
    auto start = system_clock::now();
    for (int i = 0; i < 4000000; i++) {
        f(i);
    }
    auto ms = duration_cast<milliseconds>(system_clock::now() - start);
    cout << "took " << ms.count() << endl;
    return ms.count();
};

int main(int argc, char** argv) {
    for (int i = 0; i < 10; i++) {
        run(fb1);
        run(fb2a);
        run(fb2b);
        run(fb3);
        run(fb4);
        cout << endl << endl;
    }
}
