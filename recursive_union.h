#pragma once

#include "details.h"

template <typename ...Alternatives>
struct variant;

namespace details {

struct empty_ctor_type;

template <bool trivial, typename ...Ts>
union recursive_union;

template <bool trivial>
union recursive_union<trivial> {
    constexpr recursive_union() = default;
    constexpr recursive_union(const details::empty_ctor_type&x) {};
};

template <typename Head, typename ...Tail>
union recursive_union<true, Head, Tail...> {
    ~recursive_union() = default;
    constexpr recursive_union(const details::empty_ctor_type& x) :tail(x) {}
    constexpr recursive_union() noexcept(
        std::is_nothrow_default_constructible_v<Head>) requires details::DefaultConstructible<Head> : head() {}

    template <std::size_t index, typename ...Ts>
    constexpr recursive_union(const in_place_index_t<index>, Ts&& ...t)
        : tail(in_place_index<index - 1>, std::forward<Ts>(t)...) {}

    template <typename ...Ts>
    constexpr recursive_union(const in_place_index_t<0>, Ts&& ...t) : head(std::forward<Ts>(t)...) {}

    template <std::size_t index>
    constexpr const auto& get(const in_place_index_t<index>&) {
        return tail.template get<index - 1>(in_place_index<index - 1>);
    }

    template <std::size_t index>
    constexpr const auto& get(const in_place_index_t<index>&) const {
        return tail.template get<index - 1>(in_place_index<index - 1>);
    }

    template <std::size_t index_ = 0>
    constexpr const auto& get(const in_place_index_t<0>&) {
        return head;
    }

    template <std::size_t index_ = 0>
    constexpr const auto& get(const in_place_index_t<0>&) const {
        return head;
    }

    template <std::size_t index, typename ...Args>
    constexpr auto& emplace(const in_place_index_t<index>&, Args&& ...args) {
        return tail.template emplace<index - 1, Args...>(in_place_index<index - 1>, std::forward<Args>(args)...);
    }

    template <std::size_t index = 0, typename ...Args>
    constexpr auto& emplace(const in_place_index_t<0>&, Args&& ...args) {
        new (const_cast<std::remove_cv_t<Head>*>(std::addressof(head))) Head(std::forward<Args>(args)...);
        return head;
    }

    Head head;
    recursive_union<true, Tail...> tail;
};

template <typename Head, typename ...Tail>
union recursive_union<false, Head, Tail...> {
    ~recursive_union() {}
    constexpr recursive_union(const details::empty_ctor_type& x) :tail(x) {}
    constexpr recursive_union() noexcept(
        std::is_nothrow_default_constructible_v<Head>) requires details::DefaultConstructible<Head> : head() {}

    template <std::size_t index, typename ...Ts>
    constexpr recursive_union(const in_place_index_t<index>&, Ts&& ...t)
        : tail(in_place_index<index - 1>, std::forward<Ts>(t)...) {}

    template <typename ...Ts>
    constexpr recursive_union(const in_place_index_t<0>&, Ts&& ...t) : head(std::forward<Ts>(t)...) {}

    template <std::size_t index>
    constexpr const auto& get(const in_place_index_t<index>&) {
        return tail.template get<index - 1>(in_place_index<index - 1>);
    }

    template <std::size_t index>
    constexpr const auto& get(const in_place_index_t<index>&) const {
        return tail.template get<index - 1>(in_place_index<index - 1>);
    }

    template <std::size_t index_ = 0>
    constexpr const auto& get(const in_place_index_t<0>&) {
        return head;
    }

    template <std::size_t index_ = 0>
    constexpr const auto& get(const in_place_index_t<0>&) const {
        return head;
    }

    template <std::size_t index, typename ...Args>
    constexpr auto& emplace(const in_place_index_t<index>&, Args&& ...args) {
        return tail.template emplace<index - 1, Args...>(in_place_index<index - 1>, std::forward<Args>(args)...);
    }

    template <std::size_t index = 0, typename ...Args>
    constexpr auto& emplace(const in_place_index_t<0>&, Args&& ...args) {
        new (const_cast<std::remove_cv_t<Head>*>(std::addressof(head))) Head(std::forward<Args>(args)...);
        return head;
    }

    template <std::size_t index = 0>
    constexpr void destruct(const in_place_index_t<0>&) {
        head.~Head();
    }

    template <std::size_t index>
    constexpr void destruct(const in_place_index_t<index>&) {
        tail.template destruct<index - 1>(in_place_index<index - 1>);
    }

    Head head;
    recursive_union<false, Tail...> tail;
};

template <bool trivial, typename ...Args>
struct variant_storage;

template <typename... Args>
struct variant_storage<false, Args...> {
    recursive_union<false, Args...> storage;
    std::size_t ind{0};

    constexpr variant_storage() = default;
    constexpr variant_storage(const details::empty_ctor_type& x) : storage(x) {};
    template <std::size_t index, typename ...Ts>
    constexpr variant_storage(const in_place_index_t<index>&, Ts&& ...t)
        : storage(in_place_index<index>, std::forward<Ts>(t)...), ind(index) {}

    constexpr std::size_t index() const {
        return ind;
    }

    constexpr void reset() {
        if (ind != variant_npos) {
            visitor::visit_by_index([&storage = this->storage]<std::size_t ind>(in_place_index_t<ind>) {
                storage.template destruct<ind>(in_place_index<ind>);
            }, *this);
            ind = variant_npos;
        }
    }

    constexpr ~variant_storage() {
        reset();
    }
};

template <typename ...Args>
struct variant_storage<true, Args...> {
    recursive_union<true, Args...> storage;
    std::size_t ind{0};

    constexpr variant_storage() = default;
    constexpr variant_storage(const details::empty_ctor_type& x) : storage(x) {};

    template <std::size_t index, typename ...Ts>
    constexpr variant_storage(const in_place_index_t<index>&, Ts&& ...t)
        : storage(in_place_index<index>, std::forward<Ts>(t)...), ind(index) {}

    constexpr std::size_t index() const {
        return ind;
    }

    constexpr void reset() {
        ind = variant_npos;
    };

};

}