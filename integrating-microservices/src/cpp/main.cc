#include <iostream>
#include <chrono>
#include <easylogging++.h>
#include <future>
#include "messages.pb.h"
#include <boost/asio.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/beast.hpp>
#include <boost/beast/http.hpp>

using namespace std;
using namespace std::chrono;
using tcp = boost::asio::ip::tcp;       // from <boost/asio/ip/tcp.hpp>
namespace http = boost::beast::http;    // from <boost/beast/http.hpp>

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
    //using scc = shared_ptr<cc>;
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
        cc &is_fizz;
        cc &is_buzz;
    public:
        fizzbuzz_cc(cc &a_is_fizz, cc &a_is_buzz) : is_fizz(a_is_fizz), is_buzz(a_is_buzz) {}

        bool check(int t) override {
            return is_fizz.check(t) && is_buzz.check(t);
        }
    };
    auto fizz = fizz_cc();//make_shared<fizz_cc>();
    auto buzz = buzz_cc();//make_shared<buzz_cc>();
    auto fizz_buzz = fizzbuzz_cc(fizz, buzz);

    if (fizz_buzz.check(i)) return "FizzBuzz";
    else if (fizz.check(i)) return "Fizz";
    else if (buzz.check(i)) return "Buzz";
    else return std::to_string(i);
}

const string fb4(const int i) noexcept {
    return std::async(fb1, i).get();
}

template<typename F>
auto withIO(const string fn, const F f) {
    return [fn, f](const int i) {
        ifstream in(fn, ios::in | ios::binary);
        char buffer[1024 * 1024];
        in.rdbuf()->pubsetbuf(buffer, sizeof buffer);
        in.read(buffer, sizeof(buffer));
        in.close();
        return f(i);
    };
}

template<typename F>
auto withHttp(const F f) {
    auto const ctx = shared_ptr<boost::asio::io_context>(new boost::asio::io_context());
    auto const resolver = shared_ptr<tcp::resolver>(new tcp::resolver{*ctx});
    auto const socket = shared_ptr<tcp::socket>(new tcp::socket{*ctx}, [=](tcp::socket *s) {
        s->shutdown(tcp::socket::shutdown_both);
    });
    auto const results = resolver->resolve("localhost", "8080");
    boost::asio::connect(*socket, results.begin(), results.end());

    return [socket, f](const int i) {
        http::request<http::string_body> req{http::verb::get, "/hello.txt", 11};
        http::write(*socket, req);
        http::response<http::dynamic_body> res;
        boost::beast::flat_buffer buffer;
        http::read(*socket, buffer, res);
        return f(i);
    };
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

int main() {
    for (int i = 0; i < 10; i++) {
        run(fb1);
        run(fb2a);
        run(fb2b);
        run(fb3);
        run(fb4);
        run(withIO("/dev/zero", fb1));
        run(withIO("/tmp/zeros", fb1));
        run(withHttp(fb1));
        cout << endl << endl;
    }
}
