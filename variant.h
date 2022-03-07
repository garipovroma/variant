#pragma once

#include "recursive_union.h"
#include <utility>

template <typename T>
struct variant_size;

template <typename ...Alternatives>
struct variant;

namespace details {

template <typename T>
struct variant_size_helper;

template <typename... Types>
struct variant_size_helper<variant<Types...>> : std::integral_constant<std::size_t, sizeof...(Types)> {};

template <typename... Types, bool trivial>
struct variant_size_helper<details::variant_storage<trivial, Types...>>
    : std::integral_constant<std::size_t, sizeof...(Types)> {};

template <typename T>
struct variant_size_helper<const T> : variant_size_helper<T> {};

};

template <typename ...Types>
struct variant_size<variant<Types...>>
    : details::variant_size_helper<variant<Types...>> {};

template <typename T> struct variant_size<const T> : variant_size<T> {};

template <typename T> struct variant_size<volatile T> : variant_size<T> {};
template <typename T> struct variant_size<const volatile T> : variant_size<T> {};
// deprecated in C++ 20
// https://en.cppreference.com/w/cpp/utility/variant/variant_size

template <typename T>
inline constexpr std::size_t variant_size_v = variant_size<T>::value;

template <std::size_t I, typename T>
struct variant_alternative;

template <std::size_t I, typename ...Types>
struct variant_alternative<I, variant<Types...>> {
    using type = details::get_by_ind_t<I, Types...>;
};

template <std::size_t I, typename ...Types>
struct variant_alternative<I, const variant<Types...>> {
    using type = const details::get_by_ind_t<I, Types...>;
};


template <std::size_t I, typename ...Types>
struct variant_alternative<I, volatile variant<Types...>> {
    using type = volatile details::get_by_ind_t<I, Types...>;
};
//  ^
//  |
// deprecated in C++20
// https://en.cppreference.com/w/cpp/utility/variant/variant_alternative
//  |
//  v
template <std::size_t I, typename ...Types>
struct variant_alternative<I, const volatile variant<Types...>> {
    using type = const volatile details::get_by_ind_t<I, Types...>;
};

template <std::size_t I, typename T> using variant_alternative_t = typename variant_alternative<I, T>::type;

template<class T, typename ...Types>
constexpr bool holds_alternative(const variant<Types...>& v) noexcept {
    return details::variadic_find_t<T, Types...>;
}

struct bad_variant_access;

template<std::size_t I, typename ...Types, typename X = variant_alternative_t<I, variant<Types...>>>
constexpr variant_alternative_t<I, variant<Types...>>& get(variant<Types...>& v) {
    if (v.index() == I) {
        return const_cast<X&>(v.storage.template get<I>(in_place_index<I>));
    } else {
        throw bad_variant_access();
    }
}

template<std::size_t I, typename ...Types, typename X = variant_alternative_t<I, variant<Types...>>>
constexpr variant_alternative_t<I, variant<Types...>>&& get(variant<Types...>&& v) {
    return std::move(get<I>(v));
}
template<std::size_t I, typename ...Types>
constexpr const variant_alternative_t<I, variant<Types...>>& get(const variant<Types...>& v) {
    return get<I>(const_cast<variant<Types...>&>(v));
}

template<std::size_t I, typename ...Types>
constexpr const variant_alternative_t<I, variant<Types...>>&& get(const variant<Types...>&& v) {
    return std::move(get<I>(v));
}

template<typename T, typename ...Types, std::size_t index = details::get_ind_v<T, Types...>>
constexpr T& get(variant<Types...>& v) {
    return get<index>(v);
}

template<typename T, typename ...Types, std::size_t index = details::get_ind_v<T, Types...>>
constexpr const T& get(const variant<Types...>& v) {
    return get<index>(v);
}

template<typename T, typename ...Types, std::size_t index = details::get_ind_v<T, Types...>>
constexpr T&& get(variant<Types...>&& v) {
    return get<index>(v);
}

template<typename T, typename ...Types, std::size_t index = details::get_ind_v<T, Types...>>
constexpr const T&& get(const variant<Types...>&& v) {
    return get<index>(v);
}

template<std::size_t I, typename ...Types >
constexpr std::add_pointer_t<variant_alternative_t<I, variant<Types...>>>
get_if(variant<Types...>* pv) noexcept {
    if (pv == nullptr || (pv->index() != I)) {
        return nullptr;
    }
    auto& elem = get<I>(*pv);
    auto* res = std::addressof(elem);
    return res;
}


template<std::size_t I, typename ...Types>
constexpr std::add_pointer_t<const variant_alternative_t<I, variant<Types...>>>
get_if(const variant<Types...>* pv) noexcept {
    if (pv == nullptr || (pv->index() != I)) {
        return nullptr;
    }
    auto& elem = get<I>(*pv);
    auto* res = std::addressof(elem);
    return res;
}

template<typename T, typename ...Types, std::size_t index = details::get_ind_v<T, Types...>>
constexpr std::add_pointer_t<T> get_if(variant<Types...>* pv) noexcept {
    return get_if<index, Types...>(pv);
}

template<typename T, typename ...Types, std::size_t index = details::get_ind_v<T, Types...>>
constexpr std::add_pointer_t<const T> get_if(const variant<Types...>* pv) noexcept {
    return get_if<index, Types...>(pv);
}

template <typename ...Alternatives>
struct variant:
    details::variant_storage<(std::is_trivially_destructible_v<Alternatives> && ...), Alternatives...> {

    using storage_t =
        details::variant_storage<(std::is_trivially_destructible_v<Alternatives> && ...), Alternatives...>;

    constexpr variant() = default;

    constexpr variant(const variant &other)
    requires (details::AllCopyConstructible<Alternatives...>) : storage_t(details::empty_ctor_v) {
        if (!other.valueless_by_exception()) {
            storage_t::ind = other.index();
            visitor::visit_by_index([this, &other]<std::size_t index>(in_place_index_t<index>){
                this->storage.template emplace<index>(
                    in_place_index<index>, get<index>(other));
            }, other);
        } else {
            storage_t::ind = variant_npos;
        }
    }
    constexpr variant(const variant &other) requires (details::AllCopyConstructible<Alternatives...>
        && details::AllTriviallyCopyConstructible<Alternatives...>) = default;

    constexpr variant(variant &&other) noexcept((std::is_nothrow_move_constructible_v<Alternatives> && ...))
    requires (details::AllMoveConstructible<Alternatives...>) {
        if (!other.valueless_by_exception()) {
            storage_t::ind = other.index();
            visitor::visit_by_index([&x = *this, &other]<std::size_t index>(in_place_index_t<index>){
                using X = variant_alternative_t<index, variant>;
                x.storage.template emplace<index, X>(in_place_index<index>, get<index>(std::move(other)));
            }, *this);
        } else {
            storage_t::ind = variant_npos;
        }
    }

    constexpr variant(variant &&other) noexcept((std::is_nothrow_move_constructible_v<Alternatives> && ...))
    requires (details::AllMoveConstructible<Alternatives...>
        && details::AllTriviallyMoveConstructible<Alternatives...>) = default;

    template <typename T, typename X = decltype((details::overloader<T, Alternatives...>()(std::declval<T>()))),
              std::size_t index = details::get_ind_v<X, Alternatives...>>
    constexpr variant (T&& t) noexcept(std::is_nothrow_constructible_v<X, T>)
    requires(details::ConvertingConditions<T, X, Alternatives...>)
        : storage_t(in_place_index<index>, std::forward<T>(t)) {}

    template<typename T, typename ...Args, std::size_t index = details::get_ind_v<T, Alternatives...>>
    constexpr variant(in_place_type_t<T>, Args&&... args)
    requires(details::ExactlyOnce<T, Alternatives...> && std::is_constructible_v<T, Args...>)
        : storage_t(in_place_index<index>, std::forward<Args>(args)...) {}

    template<std::size_t I, typename ...Args, typename X = typename details::get_by_ind_t<I, Alternatives...>>
    constexpr variant(in_place_index_t<I>, Args&&... args)
    requires(I < sizeof...(Alternatives) && std::is_constructible_v<X, Args...>)
        : storage_t(in_place_index<I>, std::forward<Args>(args)...)
    {}

    constexpr variant& operator=(const variant& rhs)
    requires(details::AllCopyConstructible<Alternatives...> && details::AllCopyAssignable<Alternatives...>) {
        if (this->valueless_by_exception() && rhs.valueless_by_exception()) {
            // nothing
        } else if (!this->valueless_by_exception() && rhs.valueless_by_exception()) {
            this->reset();
        } else if (this->index() == rhs.index()) {
            visitor::visit_by_index([&x = *this, &y = rhs]<std::size_t x_ind>(in_place_index_t<x_ind>){
                x = get<x_ind>(y);
            }, *this);
        } else if (this->index() != rhs.index()) {
            try {
                visitor::visit_by_index([&x = *this, &y = rhs]<std::size_t y_ind>(in_place_index_t<y_ind>){
                    x.template emplace<y_ind>(get<y_ind>(y));
                }, rhs);
            } catch (...) {
                this->reset();
                throw;
            }
        }
        return *this;
    }

    constexpr variant& operator=(const variant& rhs)
    requires(details::AllCopyConstructible<Alternatives...> && details::AllCopyAssignable<Alternatives...>
            && details::AllTriviallyCopyConstructible<Alternatives...>
            && details::AllTriviallyCopyAssignable<Alternatives...>
            && details::AllTriviallyDestructible<Alternatives...>) = default;

    constexpr variant& operator=(variant&& rhs)
    noexcept(((std::is_nothrow_move_constructible_v<Alternatives>
            && std::is_nothrow_move_assignable_v<Alternatives>) && ...))
    requires(details::AllMoveConstructible<Alternatives...> && details::AllMoveAssignable<Alternatives...>) {
        if (this->valueless_by_exception() && rhs.valueless_by_exception()) {
            // nothing
        } else if (!this->valueless_by_exception() && rhs.valueless_by_exception()) {
            this->reset();
        } else if (this->index() == rhs.index()) {
            visitor::visit_by_index([&x = *this, &y = rhs]<std::size_t x_ind>(in_place_index_t<x_ind>){
                x = get<x_ind>(std::move(y));
            }, *this);
        } else {
            try {
                visitor::visit_by_index([&x = *this, &y = rhs]<std::size_t y_ind>(in_place_index_t<y_ind>){
                    x.template emplace<y_ind>(get<y_ind>(std::move(y)));
                }, rhs);
            } catch (...) {
                this->reset();
                throw;
            }
        }
        return *this;
    }

    constexpr variant& operator=(variant&& rhs)
    noexcept(((std::is_nothrow_move_constructible_v<Alternatives>
                && std::is_nothrow_move_assignable_v<Alternatives>) && ...))
    requires(details::AllMoveConstructible<Alternatives...> && details::AllMoveAssignable<Alternatives...>
            && details::AllTriviallyMoveAssignable<Alternatives...>
            && details::AllTriviallyMoveConstructible<Alternatives...>
            && details::AllTriviallyDestructible<Alternatives...>) = default;

    template <typename T, typename X = decltype((details::overloader<T, Alternatives...>()(std::declval<T>()))),
              std::size_t index = details::get_ind_v<X, Alternatives...>>
    constexpr variant& operator=(T&& t)
    noexcept(std::is_nothrow_assignable_v<X&, T> && std::is_nothrow_constructible_v<X, T>)
    requires(details::ConvertingConditions<T, X, Alternatives...>) {
        if (index == this->ind) {
            get<index>(*this) = std::forward<T>(t);
        } else if (std::is_nothrow_constructible_v<X, T> || !std::is_nothrow_move_constructible_v<X>) {
            this->emplace<index>(std::forward<T>(t));
        } else {
            this->emplace<index>(X(std::forward<T>(t)));
        }
        this->ind = index;
        return *this;
    }

    template <typename T, typename ...Args>
    constexpr T& emplace(Args&&... args)
    requires (std::is_constructible_v<T, Args...> && details::ExactlyOnce<T, Alternatives...>) {
        const std::size_t index = details::get_ind_v<T, Alternatives...>;
        return this->emplace<index>(std::forward<Args>(args)...);
    }

    template <std::size_t index, typename ...Args, typename X = variant_alternative_t<index, variant>>
    constexpr X& emplace(Args&&... args)
    requires (std::is_constructible_v<X, Args...> && index < sizeof...(Alternatives)) {
        this->reset();
        X& res = this->storage.template emplace<index>(in_place_index<index>, std::forward<Args>(args)...);
        this->ind = index;
        return res;
    }

    constexpr void swap(variant& rhs)
        noexcept(((std::is_nothrow_move_constructible_v<Alternatives>
            && std::is_nothrow_swappable_v<Alternatives>) && ...)) {
        if (this->valueless_by_exception() && rhs.valueless_by_exception()) {
            return;
        }
        std::size_t rhs_index = rhs.index();
        if (this->index() == rhs_index) {
            visitor::visit_by_index(
                [&x = *this, &y = rhs] <std::size_t x_ind>
                (in_place_index_t<x_ind>){
                    using std::swap;
                    swap(get<x_ind>(x), get<x_ind>(y));
                }, rhs);
        } else {
            using std::swap;
            swap(rhs, *this);
        }
    }

    constexpr bool valueless_by_exception() const {
        return storage_t::ind == variant_npos;
    }

    template <bool trivial, typename ...Types>
    friend struct details::variant_storage;

};

template<typename ...Types>
constexpr bool operator==( const variant<Types...>& v,
                          const variant<Types...>& w ) {
    if (v.index() != w.index()) {
        return false;
    } else if (v.valueless_by_exception()) {
        return true;
    } else {
        return visitor::visit_by_index([&v, &w]<std::size_t index>(in_place_index_t<index>){
            return get<index>(v) == get<index>(w);
        }, v);
    }
}

template<typename ...Types>
constexpr bool operator!=(const variant<Types...>& v,
                          const variant<Types...>& w) {
    if (v.index() != w.index()) {
        return true;
    } else if (v.valueless_by_exception()) {
        return false;
    } else {
        return visitor::visit_by_index([&v, &w]<std::size_t index>(in_place_index_t<index>){
            return get<index>(v) != get<index>(w);
        }, v);
    }
}

template<class ...Types>
constexpr bool operator<(const variant<Types...>& v,
                         const variant<Types...>& w) {
    if (w.valueless_by_exception()) {
        return false;
    } else if (v.valueless_by_exception()) {
        return true;
    } else if (v.index() < w.index()) {
        return true;
    } else if (v.index() > w.index()) {
        return false;
    } else {
        return visitor::visit_by_index([&v, &w]<std::size_t index>(in_place_index_t<index>){
            return get<index>(v) < get<index>(w);
        }, v);
    }
}


template<typename ...Types>
constexpr bool operator>(const variant<Types...>& v,
                         const variant<Types...>& w) {
    if (v.valueless_by_exception()) {
        return false;
    } else if (w.valueless_by_exception()) {
        return true;
    } else if (v.index() > w.index()) {
        return true;
    } else if (v.index() < w.index()) {
        return false;
    } else {
        return visitor::visit_by_index([&v, &w]<std::size_t index>(in_place_index_t<index>){
            return get<index>(v) > get<index>(w);
        }, v);
    }
}

template<typename ...Types>
constexpr bool operator<=(const variant<Types...>& v,
                          const variant<Types...>& w) {
    if (v.valueless_by_exception()) {
        return true;
    } else if (w.valueless_by_exception()) {
        return false;
    } else if (v.index() < w.index()) {
        return true;
    } else if (v.index() > w.index()) {
        return false;
    } else {
        return visitor::visit_by_index([&v, &w]<std::size_t index>(in_place_index_t<index>){
            return get<index>(v) <= get<index>(w);
        }, v);
    }
}

template<typename ...Types>
constexpr bool operator>=(const variant<Types...>& v,
                          const variant<Types...>& w) {
    if (w.valueless_by_exception()) {
        return true;
    } else if (v.valueless_by_exception()) {
        return false;
    } else if (v.index() > w.index()) {
        return true;
    } else if (v.index() < w.index()) {
        return false;
    } else {
        return visitor::visit_by_index([&v, &w]<std::size_t index>(in_place_index_t<index>){
            return get<index>(v) >= get<index>(w);
        }, v);
    }
}
