#include <chrono>
#include <iostream>
#include <thread>

using namespace std::chrono_literals;

template <
    class result_t   = std::chrono::seconds,
    class clock_t    = std::chrono::steady_clock,
    class duration_t = std::chrono::seconds
>
auto since(std::chrono::time_point<clock_t, duration_t> const& start)
{
    return std::chrono::duration_cast<result_t>(clock_t::now() - start);
}

int main(void){
    int x;

    auto start = std::chrono::steady_clock::now();
    
    //std::this_thread::sleep_for(10ms);
    
    start = std::chrono::steady_clock::now();
    //std::this_thread::sleep_for(1ms);
 
    //while loop
    x = 0;
    while(x < 1000000009){
        x += 1; // equivilant x = x + 1; and x++;

    }

    //Do-while
    x = 0;
    do{
        ++x;
    } while (x < 1000000009);

    //For
    for( x = 0 ; x< 1000000009 ; ++x ){

    } 
    std::cout << "Elapsed(ms)=" << since(start).count() << std::endl;
    //std::cout << "Elapsed(s)="
    //    << since<std::chrono::seconds>(start).count() << std::endl;
}